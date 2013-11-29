package mware_lib.skeleton;

import mware_lib.object_server.ObjectServerMessage;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Socket;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 29.11.13
 * Time: 15:26
 */
public class SkeletonImpl implements Skeleton {
    Object servant;
    SkeletonFactory factory;

    ObjectServerMessage objectServerMessage;
    int id;

    Socket clientSocket;
    ObjectOutputStream objOS;
    ObjectInputStream objIS;

    SkeletonImpl(SkeletonFactory factory,Object servant, ObjectServerMessage objectServerMessage, int id, Socket clientSocket) {
        this.factory = factory;
        this.servant = servant;
        this.objectServerMessage = objectServerMessage;
        this.id = id;
        this.clientSocket = clientSocket;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SkeletonImpl that = (SkeletonImpl) o;

        if (id != that.id) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id;
    }
    @Override
    public Object callFunction(ObjectServerMessage serviceMessage) {
        Method[] methods = servant.getClass().getMethods();
        Object returnValue = null;

        for (Method method : methods) {
            if (method.getName().equals(String.valueOf(serviceMessage.getOperation()))) {
                try {
                    returnValue = method.invoke(servant, serviceMessage.getParameter());
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }
        return returnValue;
    }

    @Override
    public String toString() {
        return "SkeletonImpl{" +
                "id=" + id +
                ", objectServerMessage=" + objectServerMessage +
                ", factory=" + factory +
                ", servant=" + servant +
                '}';
    }
}
