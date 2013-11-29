package bank_access;

import mware_lib.Config;
import mware_lib.object_server.ObjectServerMessage;
import mware_lib.stub.Stub;
import mware_lib.stub.StubImpl;
import mware_lib.name_server.NameServiceMessage;
import mware_lib.stub.StubImplReuseConnection;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:45
 */
public class AccountImplStub extends AccountImplBase {
    Stub stub;

    public AccountImplStub(NameServiceMessage nameServiceMessage) {
        if(Config.DEBUG) System.out.println("AccountImplStub created");
        this.stub = new StubImpl(nameServiceMessage);
//        this.stub = new StubImplReuseConnection(nameServiceMessage);
    }

    @Override
    public void transfer(double amount) throws OverdraftException {
            stub.sendObjectServerMessage(
                    new ObjectServerMessage("transfer", new Double[]{amount})
            );
    }

    @Override
    public double getBalance()  {
        double returnValue = 0;
        try {
            returnValue = ((Double) stub.sendObjectServerMessage(
                                                            new ObjectServerMessage("getBalance", new Object[]{})
                                                            ).getReturnVal()
                          )
                          .doubleValue();

        } catch (OverdraftException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            throw new UnknownError();
        }

        return returnValue;
    }
}
