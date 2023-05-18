package be.funp.fun_in_java_intellij;

/**
 * Created by u0098595 on 2/02/2016.
 */
public class BonusGiver {
    public String name;
    public double amount;

    public BonusGiver(String name, double amount){
        this.name = name;
        this.amount = amount;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public double getAmount() {
        return amount;
    }

    public void setAmount(double amount) {
        this.amount = amount;
    }
}
