package mware_lib.stub;

import bank_access.OverdraftException;
import mware_lib.Config;
import mware_lib.NameServiceMessage;
import mware_lib.object_server.ObjectServerMessage;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 29.11.13
 * Time: 16:03
 */
public class StubImpl implements Stub {
    NameServiceMessage nameServiceMessage;

    public StubImpl(NameServiceMessage nameServiceMessage) {
        if (Config.DEBUG) System.out.println("StubImpl::created");
        this.nameServiceMessage = nameServiceMessage;
    }

    @Override
    public ObjectServerMessage sendObjectServerMessage(ObjectServerMessage message) throws OverdraftException {
        ObjectServerMessage answer = null;
        message.setObjectID(nameServiceMessage.getId());
        message.setMsg(ObjectServerMessage.Msg.CALL_ON_OBJECT);

        try {
            final Socket objectServer = new Socket(nameServiceMessage.getInetAddress(), nameServiceMessage.getPort());
            final ObjectOutputStream objOS = new ObjectOutputStream(objectServer.getOutputStream());
            final ObjectInputStream objIS = new ObjectInputStream(objectServer.getInputStream());

            objOS.writeObject(message);
            answer = (ObjectServerMessage) objIS.readObject();

            switch (answer.getMsg()) {
                case OBJECT_NOT_FOUND:
                    throw new OverdraftException("ImplSkeleton on ObjectServer notfound");
                case OBJECT_FOUND:
                    if(Config.DEBUG) System.out.println("ImplStub:: Object_Found");
                    break;
            }
            // close connection
            objOS.writeObject(new ObjectServerMessage(ObjectServerMessage.Msg.CLOSE_CON));

            objIS.close();
            objOS.close();
            objectServer.close();

        } catch (ClassNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return answer;
    }
}
