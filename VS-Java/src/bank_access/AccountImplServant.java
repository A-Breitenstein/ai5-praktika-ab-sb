package bank_access;

import mware_lib.Config;
import mware_lib.ObjectServerMessage;
import mware_lib.Servant;
import mware_lib.Skeleton;

import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 27.11.13
 * Time: 20:47
 */
public class AccountImplServant extends AccountImplBase implements Servant {
    double account;
    List<Skeleton> skeletons = new ArrayList<Skeleton>();
    Semaphore skeleton_per_servant = new Semaphore(Config.SKELETONS_PER_SERVANT,true);

    public AccountImplServant(double account) {
        this.account = account;
    }

    @Override
    public void transfer(double amount) throws OverdraftException {
        account += amount;
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
    public Skeleton createSkeleton(Socket clientSocket, ObjectServerMessage serviceMessage) {
        Skeleton tmp = null;
        try {
            System.out.println("permits:"+skeleton_per_servant.availablePermits());
            skeleton_per_servant.acquire();
            tmp = new AccountImplSkeleton(this, clientSocket, serviceMessage, skeletons.size());
            skeletons.add(tmp);
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

        return tmp;
    }

    @Override
    public void removeSkeleton(Skeleton skeleton) {
        skeletons.remove(skeleton);
        skeleton_per_servant.release();
    }

    @Override
    public int getReferences() {
        return skeletons.size();
    }
}
