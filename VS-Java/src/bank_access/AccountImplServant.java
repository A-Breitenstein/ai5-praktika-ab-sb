package bank_access;

import mware_lib.Config;
import mware_lib.ObjectServerMessage;
import mware_lib.Servant;
import mware_lib.Skeleton;

import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:47
 */
public class AccountImplServant extends AccountImplBase implements Servant{
    double account;
    List<Skeleton> skeletons = new ArrayList<Skeleton>();

    public AccountImplServant(double account) {
        this.account = account;
    }

    @Override
    public void transfer(double amount) throws OverdraftException {
        account+=amount;
    }

    @Override
    public double getBalance() {
        return account;
    }

    @Override
    public String toString() {
        return "AccountImplServant{" +
                "account=" + account +
                '}';
    }

    @Override
    public synchronized Skeleton createSkeleton(Socket clientSocket, ObjectServerMessage serviceMessage) {
        Skeleton tmp = null;
        if (skeletons.size() < Config.SKELETONS_PER_SERVANT) {
            tmp = new AccountImplSkeleton(this, clientSocket, serviceMessage, skeletons.size());
            skeletons.add(tmp);
        }
        return tmp;
    }

    @Override
    public synchronized void removeSkeleton(Skeleton skeleton) {
        skeletons.remove(skeleton);
    }
}
