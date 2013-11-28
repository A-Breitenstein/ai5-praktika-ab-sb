package applications;

import java.io.BufferedInputStream;
import java.io.IOException;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 12:59
 * To change this template use File | Settings | File Templates.
 */
public class BankStarter {
    public static void main(String[] args) {
        Process cat = null;
        try {
//            cat = Runtime.getRuntime().exec("java -jar C:\\Users\\Sven\\IdeaProjects\\VS-Java\\src\\applications\\bank.jar ");
            cat = Runtime.getRuntime().exec("java -cp C:\\Users\\Sven\\IdeaProjects\\VS-Java\\src\\bank.jar bank.Bank localhost 50004 qwe123 -v");
            BufferedInputStream catOutput= new BufferedInputStream(cat.getInputStream());
            int read = 0;
            byte[] output = new byte[1024];
            while ((read = catOutput.read(output)) != -1) {
                System.out.println(output[read]);
            }
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }
}
