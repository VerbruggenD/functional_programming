module be.funp.fun_in_java_intellij {
    requires javafx.controls;
    requires javafx.fxml;
            
        requires org.controlsfx.controls;
                            
    opens be.funp.fun_in_java_intellij to javafx.fxml;
    exports be.funp.fun_in_java_intellij;
}