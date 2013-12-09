package bank_access;

import mware_lib.Config;
import mware_lib.NameServiceMessage;
import mware_lib.object_server.ObjectServerMessage;
import mware_lib.stub.Stub;
import mware_lib.stub.StubFactory;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 13:30
 * To change this template use File | Settings | File Templates.
 */
public class ManagerImplStub extends ManagerImplBase {
    Stub stub;

    public ManagerImplStub(NameServiceMessage nameServiceMessage) {
        if(Config.DEBUG) System.out.println("ManagerImplStub created");
        this.stub = StubFactory.createStub(nameServiceMessage);

    }

    @Override
    public String createAccount(String owner, String branch) {

        try {
            return (String)(stub.sendObjectServerMessage(new ObjectServerMessage("createAccount", new Object[]{owner, branch})).getReturnVal());
        } catch (OverdraftException e) {
            e.printStackTrace();
            throw new UnknownError();
        }
    }
}
