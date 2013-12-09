package cash_access;

import bank_access.OverdraftException;
import mware_lib.Config;
import mware_lib.NameServiceMessage;
import mware_lib.object_server.ObjectServerMessage;
import mware_lib.stub.Stub;
import mware_lib.stub.StubFactory;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 29.11.13
 * Time: 17:08
 */
public class TransactionImplStub extends TransactionImplBase {
    Stub stub;
    public TransactionImplStub(NameServiceMessage nameServiceMessage) {
        if(Config.DEBUG) System.out.println("TransactionImplStub created");
        this.stub = StubFactory.createStub(nameServiceMessage);


    }

    @Override
    public void deposit(String accountId, double amount) throws InvalidParamException {
        try {
            stub.sendObjectServerMessage(
                    new ObjectServerMessage("deposit",
                    new Object[]{accountId,new Double(amount)})
            );
        } catch (OverdraftException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void withdraw(String accountId, double amount) throws InvalidParamException, OverdraftException {
        stub.sendObjectServerMessage(
                new ObjectServerMessage("withdraw",
                new Object[]{accountId,new Double(amount)})
        );
    }

    @Override
    public double getBalance(String accountId) throws InvalidParamException {
        try {
            return ((Double) ((stub.sendObjectServerMessage(
                                     new ObjectServerMessage("getBalance",
                                     new Object[]{accountId}
                              ))).getReturnVal())).longValue();
        } catch (OverdraftException e) {
            e.printStackTrace();
        }
        throw new UnknownError();
    }
}
