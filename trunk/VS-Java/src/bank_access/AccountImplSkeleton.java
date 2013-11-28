package bank_access;

import mware_lib.ObjectServerMessage;
import mware_lib.Skeleton;
import mware_lib.SkeletonFactory;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:47
 */
public class AccountImplSkeleton extends AccountImplBase implements Skeleton {
    SkeletonFactory accountSkeletonFactory;
    Socket clientSocket;
    ObjectServerMessage objectServerMessage;
    int id;

    ObjectOutputStream objOS;
    ObjectInputStream objIS;

    public AccountImplSkeleton(SkeletonFactory skeletonFactory, Socket clientSocket, ObjectServerMessage objectServerMessage, int id) {
        this.accountSkeletonFactory = skeletonFactory;
        this.clientSocket = clientSocket;
        this.objectServerMessage = objectServerMessage;
        this.id = id;
    }

    @Override
    public void transfer(double amount) throws OverdraftException {
        throw new UnsupportedOperationException("AccountImplSkeleton :: transfer not implemented yet");
    }

    @Override
    public double getBalance() {
        throw new UnsupportedOperationException("AccountImplSkeleton :: getBalance not implemented yet");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        AccountImplSkeleton that = (AccountImplSkeleton) o;

        if (id != that.id) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public Object callFunction(ObjectServerMessage serviceMessage) {
        Method[] methods = accountSkeletonFactory.getClass().getMethods();
        Object returnValue = null;

        for (Method method : methods) {

            if (method.getName().equals(String.valueOf(serviceMessage.getOperation()))) {
                try {
                    returnValue = method.invoke(accountSkeletonFactory, serviceMessage.getParameter());
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }
        return returnValue;
    }
}
