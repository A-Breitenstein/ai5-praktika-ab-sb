package mware_lib.object_server;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:37
 */
public class ObjectServerMessage implements Serializable{
    Object operation;
    Object[] parameter;
    String objectID;
    Object returnVal;
    Msg msg;

    public ObjectServerMessage(Object operation, Object[] parameter) {
        this.operation = operation;
        this.parameter = parameter;
    }

    public enum Msg {
        OBJECT_NOT_FOUND,
        OBJECT_FOUND;
    }

    public ObjectServerMessage(Object operation, Object[] parameter, String objectID) {
        this(operation, parameter);
        this.objectID = objectID;
    }

    @Override
    public String toString() {
        return "ObjectServerMessage{" +
                "operation=" + operation +
                ", parameter=" + Arrays.toString(parameter) +
                ", objectID='" + objectID + '\'' +
                ", returnVal=" + returnVal +
                ", msg=" + msg +
                '}';
    }

    public Object getOperation() {
        return operation;
    }

    public void setOperation(Object operation) {
        this.operation = operation;
    }

    public Object[] getParameter() {
        return parameter;
    }

    public void setParameter(Object[] parameter) {
        this.parameter = parameter;
    }

    public String getObjectID() {
        return objectID;
    }

    public void setObjectID(String objectID) {
        this.objectID = objectID;
    }

    public Object getReturnVal() {
        return returnVal;
    }

    public void setReturnVal(Object returnVal) {
        this.returnVal = returnVal;
    }

    public Msg getMsg() {
        return msg;
    }

    public void setMsg(Msg msg) {
        this.msg = msg;
    }
}
