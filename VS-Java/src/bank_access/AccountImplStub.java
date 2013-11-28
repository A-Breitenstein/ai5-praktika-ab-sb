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
    NameServiceMessage nameServiceMessage;

    public AccountImplStub(NameServiceMessage nameServiceMessage) {
        this.nameServiceMessage = nameServiceMessage;

    }

    private ObjectServerMessage sendObjectServerMessage(ObjectServerMessage message) throws OverdraftException {
        ObjectServerMessage answer = null;
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
                    System.out.println("AccountImplStub:: Object_Found");
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

    @Override
    public void transfer(double amount) throws OverdraftException {
            sendObjectServerMessage(new ObjectServerMessage("transfer", new Double[]{amount}, nameServiceMessage.getId()));
    }

    @Override
    public double getBalance()  {
        double returnValue = 0;
        try {
            returnValue = ((Double) sendObjectServerMessage(
                                                            new ObjectServerMessage("getBalance", new Object[]{}, nameServiceMessage.getId())
                                                            ).getReturnVal()
                          )
                          .doubleValue();

        } catch (OverdraftException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

        return returnValue;
    }
}
