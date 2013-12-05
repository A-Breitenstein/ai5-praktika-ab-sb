package mware_lib.stub;

import bank_access.OverdraftException;
import mware_lib.Config;
import name_service.NameServiceMessage;
import mware_lib.object_server.ObjectServerMessage;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 29.11.13
 * Time: 19:21
 */
public class StubImplReuseConnection implements Stub {
    private  Socket objectServer;
    private ObjectOutputStream objOS;
    private ObjectInputStream objIS;
    NameServiceMessage nameServiceMessage;

    public StubImplReuseConnection(NameServiceMessage nameServiceMessage) {
        if (Config.DEBUG) System.out.println("StubImpl::created");
        this.nameServiceMessage = nameServiceMessage;
        try {
            objectServer = new Socket(nameServiceMessage.getInetAddress(), nameServiceMessage.getPort());
            objOS = new ObjectOutputStream(objectServer.getOutputStream());
            objIS = new ObjectInputStream(objectServer.getInputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    protected void finalize() throws Throwable {
        objIS.close();
        objOS.close();
        objectServer.close();
        super.finalize();    //To change body of overridden methods use File | Settings | File Templates.
    }

    @Override
    public ObjectServerMessage sendObjectServerMessage(ObjectServerMessage message) throws OverdraftException {
        ObjectServerMessage answer = null;
        message.setObjectID(nameServiceMessage.getId());
        try {
            objOS.writeObject(message);
            answer = (ObjectServerMessage) objIS.readObject();

            switch (answer.getMsg()) {
                case OBJECT_NOT_FOUND:
                    throw new OverdraftException("ImplSkeleton on ObjectServer notfound");
                case OBJECT_FOUND:
                    if(Config.DEBUG) System.out.println("ImplStub:: Object_Found");
                    break;
            }

        } catch (ClassNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return answer;
    }
}
