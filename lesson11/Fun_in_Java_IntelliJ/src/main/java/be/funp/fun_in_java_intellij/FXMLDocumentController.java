package be.funp.fun_in_java_intellij;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;

import java.util.*;
import java.util.function.ToDoubleFunction;

public class FXMLDocumentController {

    @FXML
    private Button selectFixedData;

    @FXML
    private Button selectRandomData;

    @FXML
    private Button ageSorter;

    @FXML
    private Button salarySorter;

    @FXML
    private Button btnExercise1;

    @FXML
    private Button btnExercise2;

    @FXML
    private Button btnExercise3;

    @FXML
    private Button btnExercise4;

    @FXML
    private Button btnExercise5;

    @FXML
    private Button btnExercise6;

    @FXML
    private TextArea status;

    @FXML
    private TextArea output;

    private List<Employee> employees;
    private List<Bonus> boni;


    
    @FXML
    void initialize() {
        populateTestData();
        //randomData();
        selectFixedData.setOnAction(e->fixedData());
        selectRandomData.setOnAction(e->randomData());
        ageSorter.setOnAction(e->sortByAge());
        salarySorter.setOnAction(e->sortBySalary());
        btnExercise1.setOnAction(e->exercise1());
        btnExercise2.setOnAction(e->exercise2());
        btnExercise3.setOnAction(e->exercise3());
        btnExercise4.setOnAction(e->exercise4());
        btnExercise5.setOnAction(e->exercise5());
        btnExercise6.setOnAction(e->exercise6());
    }

    /*****************************
     *                           *
     *  FIRST SET OF EXERCISES   *
     *                           *
     *****************************/


    /**
     * Exercise A: sort by age
     */
    public void sortByAge() {
        status.clear();

        Collections.sort(employees,
                (x,y) -> x.getAge() - y.getAge()
        );  // can aangepast worden door intellij door rechtermuisknop vervang

        showData();
    }

    /**
     * Exercise B: sort by salary
     */
    public void sortBySalary() {
        status.clear();

        Collections.sort(employees,
                (x,y) -> (int) (x.getSalary() - y.getSalary())
        );

        showData();
    }
    /**
     * Exercise C: write a generic method sortBy that takes a comparator between two employees.
     *             Use this function in the initialize-method to sort the employees by name
     * @param employeeComparator function that compares 2 employees
     */
    public void sortBy(Comparator<Employee> employeeComparator) {
        status.clear();

        Collections.sort(employees, employeeComparator);

        showData();
    }

    // a different version of the function that uses a function that converts an Employee to a double, and that uses Comparator.comparingDouble
    public void sortBy(ToDoubleFunction<Employee> employee2DoubleFunction) {
        status.clear();

        Collections.sort(employees, Comparator.comparingDouble(employee2DoubleFunction));

        showData();
    }

    /*****************************
     *                           *
     *  SECOND SET OF EXERCISES  *
     *                           *
     *****************************/


    /**
        Exercise 1: update the following code to count the employees who earn well
    */
    private void exercise1(){
        output.clear();

        long n employees.stream().filter(employee -> employee.getSalary() > 4400).count();

//        int n = 0;
//        for (Employee empl : employees)
//        {
//            if (empl.getSalary() > 4400)
//            {
//                n++;
//            }
//        }

        double average = 0;
        for (Employee empl : employees)
        {
            average += empl.getSalary();
        }
        average = average / employees.size();

        int n2 = 0;
        for (Employee empl : employees)
        {
            if (empl.getSalary() > average)
            {
                n2++;
            }
        }

        output.setText( n + " people earn more than 4.4K\n" +
                n2 + " people earn more than average (" + average + ")\n");
    }

    /**
       Exercise 2 with 3 subtasks
    */
    private void exercise2(){
        output.clear();
        List<Double> list = new ArrayList<>();

        /**
         Exercise 2a: - update this loop to perform
         1. a mapping operation on each of the employees
         to obtain a stream of ratio's (salary-1000)/age
         2. collect the maximum from the resulting list

         - update the second loop to consume the max salary using a lambda expression

         - update the third loop to find the best paid employee by using an aggregate

         */
        for (Employee empl : employees)
        {
            double ratio = (empl.getSalary() - 1000) / empl.getAge();
            list.add(ratio);
        }

        double topRatio = 0;
        for (double d : list)
        {
            if (d > topRatio) topRatio = d;
        }

        /**
         Exercise 2b: update this loop to consume the max salary using a lambda expression
         */
        double bestSalary = employees.get(0).getSalary();
        for (Employee empl : employees)
        {
            if (empl.getSalary() > bestSalary)
            {
                bestSalary = empl.getSalary();
            }
        }


        /**
         Exercise 2c: update this loop to find the best paid employee (given the max salary, as found beforehand)
         */
        Employee best = employees.get(0);
        for (Employee empl : employees)
        {
            if (empl.getSalary() > best.getSalary())
            {
                best = empl;
            }
        }

        output.setText("The highest ratio of wages versus age is " + topRatio + "\n"
                + "The best salary is " + bestSalary + "\n"
                + "paid to " + best + "\n");

    }

    /**
       Exercise 3: write code to find the person who nominated for the largest amount
    */
    private void exercise3(){
        output.clear();
        List<BonusGiver> donators = new ArrayList<>();
        for (Bonus b : boni)
        {
            int id = b.getNominatorId();
            for (Employee empl : employees)
            {
                if (empl.getId() == id)
                {
                    donators.add(new BonusGiver(empl.getName(), b.getAmount()));
                }
            }
        }

        StringBuilder content = new StringBuilder();

        // following code already uses a lambda
        donators.stream().forEach(d -> content.append(d.getName() + " nominated a bonus worth " + d.getAmount() + "\n"));
        status.setText(content.toString());

        /**Insert code here: write code to find the person who nominated for the largest amount*/
        String employeeName = "";

        output.setText(employeeName);
    }

    /**
     exercise 4: Write a function that takes a list of employees and a minimum salary and that returns
                the first employee earning more than that minimum.
                Wrap the value into the Option type.

                Write code that uses the function and that checks whether such an employee exists.

        @param employees the list of employees to look in
        @param minSalary the salary that acts as a lower limit for the employee to be selected

        @return the first (if any) employee with a salary exceeding the minSalary
     */
    private Optional<Employee> findFirstEmployeeRicherThan(List<Employee> employees, double minSalary) {
        return null;
    }


    private void exercise4(){
        output.clear();

        Optional<Employee> maybeSomeOne = findFirstEmployeeRicherThan(employees, 5500);
        if (maybeSomeOne.isPresent()) {
            Employee richOne = maybeSomeOne.get();
            output.setText(richOne + " earns more than 5.5K: " +  richOne.getSalary() + "\n");
        }
        else {
            output.setText("Nobody earns more than 5.5K\n");
        }

    }

    /**
     Exercise 5: Replace the loop below by an appropriate use of streams.
            Use Stream.reduce method and use method referencing wherever possible.

            Check carefully whether you preserve the original behavior.
     */
    private void exercise5(){
        output.clear();

        StringBuilder buffer = new StringBuilder();
        for (Employee p : employees) {
            buffer.append(p.toString());
            buffer.append('\n');
        }

        output.setText(buffer.toString());
    }

    /**
     Exercise 6: Solve the three questions below
     highest bonus, all employees older than 30, employee that earn the most
     */
    private void exercise6(){
        output.clear();

        /**1. Find the highest bonus in the bonus list */
        double highestBonus = 0;

        /**2. Find all employees that are older than 30 */
        List<Employee> oldEmployees = new ArrayList<>();

        /**3. Find the employee that earns the most */
        String richest = "Nobody";



        StringBuilder oldEmpls = new StringBuilder();
        oldEmployees.stream().forEach(e -> oldEmpls.append("Name " + e.getName() + ", Age " + e.getAge()+ "\n"));
        output.setText("Highest bonus: " + highestBonus + "\n"
                + " Old Employees " + oldEmpls.toString() + ", Richest: " + richest);
    }

    private void fixedData(){
        status.clear();
        populateTestData();
        showData();
    }

    private void randomData(){
        status.clear();
        populateTestDataDynamically();
        showData();
    }

    private void showData(){
        StringBuilder text = new StringBuilder();
        text.append("");
        employees.stream().forEach(e-> text.append(e.toString() + "\n"));
        boni.stream().forEach(b-> text.append(b.toString() + "\n"));
        status.setText(text.toString());
    }

    private void populateTestData()
    {
        Employee.resetCounter();
        employees = new ArrayList<>();
        employees.add(new Employee("Atifa", 20, 3400));
        employees.add(new Employee("Lucas", 30, 4400));
        employees.add(new Employee("Adam", 40, 5400));
        employees.add(new Employee("Hanne", 50, 6400));

        boni = new ArrayList<>();
        boni.add(new Bonus(500, employees.get(0).getId(), employees.get(0).getId() + 1, "blabla"));
        boni.add(new Bonus(600, employees.get(0).getId(), employees.get(0).getId() + 2, "more blabla"));
        boni.add(new Bonus(700, employees.get(0).getId() + 1, employees.get(0).getId() + 3, "YAB"));
        boni.add(new Bonus(800, employees.get(0).getId() + 3, employees.get(0).getId()+ 2, "yet another bonus"));
    }
    private void populateTestDataDynamically()
    {
        Employee.resetCounter();
        Random rnd = new Random();
        employees = new ArrayList<>();
        employees.add(new Employee("Atifa", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Lucas", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Adam", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Hanne", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Mohamed", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Maya", rnd.nextInt(50)+10, rnd.nextInt(50)*100));
        employees.add(new Employee("Finn", rnd.nextInt(50)+10, rnd.nextInt(50)*100));

        int nrOfEmployees = employees.size();

        boni = new ArrayList<>();
        int count = rnd.nextInt(50);
        for (int i=0; i < count; i++)
        {
            int giver = rnd.nextInt(nrOfEmployees);
            int receiver = rnd.nextInt(nrOfEmployees);
            boni.add(new Bonus(rnd.nextInt(20)*50, giver, receiver, "nice job " + i));
        }
    }

}
