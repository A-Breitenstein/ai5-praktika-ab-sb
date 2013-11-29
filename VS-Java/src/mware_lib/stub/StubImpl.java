package mware_lib.stub;

import bank_access.OverdraftException;
import mware_lib.Config;
import mware_lib.object_server.ObjectServerMessage;
import mware_lib.name_server.NameServiceMessage;

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
        this.nameServiceMessage = nameServiceMessage;
    }

    @Override
    public ObjectServerMessage sendObjectServerMessage(ObjectServerMessage message) throws OverdraftException {
        ObjectServerMessage answer = null;
        message.setObjectID(nameServiceMessage.getId());

        try {
            final Socket objectServer = new Socket(nameServiceMessage.getInetAddress(), nameServiceMessage.getPort());
            final ObjectOutputStream objOS = new ObjectOutputStream(objectServer.getOutputStream());
            final ObjectInputStream objIS = new ObjectInputStream(objectServer.getInputStream());

            objOS.writeObject(message);
            answer = (ObjectServerMessage) objIS.readObject();

            switch (answer.getMsg()) {
                case OBJECT_NOT_FOUND:
                    throw new OverdraftException("AccountImplSkeleton on ObjectServer notfound");
                case OBJECT_FOUND:
                    if(Config.DEBUG) System.out.println("AccountImplStub:: Object_Found");
                    break;
            }
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
