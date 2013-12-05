package name_service;

import java.io.Serializable;
import java.net.InetAddress;

/**
 * Created with IntelliJ IDEA.
 * User: abg628
 * Date: 27.11.13
 * Time: 15:35
 * To change this template use File | Settings | File Templates.
 */
public class NameServiceMessage implements Serializable{
    public enum Operations{
        REBIND,
        RESOLVE,
        CLOSE_CON;

    }
    Operations operation;
    InetAddress inetAddress;
    int port;
    String id;

    public NameServiceMessage(Operations operation, InetAddress inetAddress, int port, String id) {
        this.operation = operation;
        this.inetAddress = inetAddress;
        this.port = port;
        this.id = id;
    }

    @Override
    public String toString() {

        switch (operation) {
            case REBIND:
                return "NameServiceMessage{" +
                        "operation=" + operation +
                        ", inetAddress=" + inetAddress +
                        ", port=" + port +
                        ", id='" + id + '\'' +
                        '}';
            case RESOLVE:
                return "NameServiceMessage{" +
                        "operation=" + operation +
                        ", id='" + id + '\'' +
                        '}';
            case CLOSE_CON:
                return "NameServiceMessage{" +
                        "operation=" + operation +
                        '}';
        }
        return null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        NameServiceMessage that = (NameServiceMessage) o;

        if (id != null ? !id.equals(that.id) : that.id != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return id != null ? id.hashCode() : 0;
    }

    public Operations getOperation() {
        return operation;
    }

    public InetAddress getInetAddress() {
        return inetAddress;
    }

    public int getPort() {
        return port;
    }

    public String getId() {
        return id;
    }
}
