package be.funp.fun_in_java_intellij;

import java.util.Comparator;

/**
 * Created by u0098595 on 2/02/2016.
 */
public class Employee {
    private int id;
    private static int maxID = 1;
    private String name;
    private int age;
    private double salary;

    public static void resetCounter() {
        maxID = 1;
    }
    
    public Employee(String name, int age, double wages){
        id = maxID;
        maxID++;
        this.name = name;
        this.age = age;
        this.salary = wages;
    }

    /**
        Exercise 7: let the setFullName return an Optional<> type, let it return empty if the input is null.
     */
    public String setFullName(String full){
        name = full;
        return name ;
    }
    @Override
    public String toString(){
        return "Employee " + id + " : " + name + ", aged " +age + ". € " + salary;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public static int getMaxID() {
        return maxID;
    }

    public static void setMaxID(int maxID) {
        Employee.maxID = maxID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public double getSalary() {
        return salary;
    }

    public void setSalary(double salary) {
        this.salary = salary;
    }
}
