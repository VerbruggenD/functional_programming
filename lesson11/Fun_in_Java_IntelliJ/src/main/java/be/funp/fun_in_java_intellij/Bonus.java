package be.funp.fun_in_java_intellij;

/**
 * Created by u0098595 on 2/02/2016.
 */
public class Bonus {
    private double amount;
    private int nomineeId;
    private int nominatorId;
    private String motivation;

    public Bonus(int amount, int whoNominates, int whoReceives, String motivation)
    {
        this.amount = amount;
        this.nominatorId = whoNominates;
        this.nomineeId = whoReceives;
        this.motivation = motivation;
    }

    public void updateAmount(double percentage)
    {
        amount = amount * percentage;
    }

    public double getAmount() {
        return amount;
    }

    @Override
    public String toString(){
        return "Bonus " + nomineeId + " : nominator" + nominatorId + ", amount " +amount + "€, motivation " + motivation;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }

    public int getNomineeId() {
        return nomineeId;
    }

    public void setNomineeId(int nomineeId) {
        this.nomineeId = nomineeId;
    }

    public int getNominatorId() {
        return nominatorId;
    }

    public void setNominatorId(int nominatorId) {
        this.nominatorId = nominatorId;
    }

    public String getMotivation() {
        return motivation;
    }

    public void setMotivation(String motivation) {
        this.motivation = motivation;
    }
}
