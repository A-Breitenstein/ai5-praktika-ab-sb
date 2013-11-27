package bank_access;

import mware_lib.ObjectServerMessage;
import mware_lib.Stub;
import name_service.NameServiceMessage;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:45
 */
public class AccountImplStub extends AccountImplBase implements Stub {
    Socket objectServer;
    NameServiceMessage nameServiceMessage;
    ObjectOutputStream objOS;
    ObjectInputStream objIS;
    public AccountImplStub(NameServiceMessage nameServiceMessage) {
        try {
            this.nameServiceMessage = nameServiceMessage;
            objectServer = new Socket(nameServiceMessage.getInetAddress(), nameServiceMessage.getPort());
            objOS = new ObjectOutputStream(objectServer.getOutputStream());
            objIS = new ObjectInputStream(objectServer.getInputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Override
    public void transfer(double amount) throws OverdraftException {
        try {

            objOS.writeObject(new ObjectServerMessage("transfer",new Object[]{(Object)Double.valueOf(amount)},nameServiceMessage.getId()));
            ObjectServerMessage answer = (ObjectServerMessage) objIS.readObject();
            switch (answer.getMsg()) {
                case OBJECT_NOT_FOUND:
                    throw new OverdraftException("AccountImplSkeleton on ObjectServer notfound");
                case OBJECT_FOUND:
                    System.out.println("AccountImplStub:: Object_Found");

                    break;
            }

        } catch (IOException e) {
            throw new OverdraftException("AccountImplStub::transfer -> IO Exception");
        } catch (ClassNotFoundException e) {
            throw new OverdraftException("AccountImplStub::transfer -> ClassNotFound Exception");
        }
    }

    @Override
    public double getBalance() {
        throw new UnsupportedOperationException("AccountImplStub :: getBalance not implemented yet");
    }
}
