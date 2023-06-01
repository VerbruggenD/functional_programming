import xs from 'xstream';
import { run } from '@cycle/run';
import { div, button, p, makeDOMDriver } from '@cycle/dom';

function main(sources) {
  const buttonPress$ = sources.DOM.select('.button').events('click');
  const resetButtonPress$ = sources.DOM.select('.reset-button').events('click');

  const initialState = { startTime: 0, maxSpeed: 0 };

  const updateSpeed = (state, timeDiff) => ({
    ...state,
    maxSpeed: Math.max(state.maxSpeed, 1000 / timeDiff),
  });

  const reducer$ = xs.merge(
    buttonPress$
      .fold((state, _) => ({ ...state, startTime: Date.now() }), initialState)
      .map((state) => {
        const timeDiff = Date.now() - state.startTime;
        return updateSpeed(state, timeDiff);
      }),

    resetButtonPress$.mapTo(initialState)
  );

  const state$ = reducer$.fold((state, reducer) => reducer(state), initialState);

  const vdom$ = state$.map(({ maxSpeed }) =>
    div([
      button('.button', 'Press me'),
      button('.reset-button', 'Reset'),
      p(`Maximum Speed: ${maxSpeed.toFixed(2)} clicks per second`),
    ])
  );

  return {
    DOM: vdom$,
  };
}

const drivers = {
  DOM: makeDOMDriver('#app'),
};

run(main, drivers);
