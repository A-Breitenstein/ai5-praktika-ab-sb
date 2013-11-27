package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: abg667
 * Date: 18.11.13
 * Time: 13:00
 * To change this template use File | Settings | File Templates.
 */
public class OverdraftException extends Exception {
    public OverdraftException(String msg) {super(msg);}
}
