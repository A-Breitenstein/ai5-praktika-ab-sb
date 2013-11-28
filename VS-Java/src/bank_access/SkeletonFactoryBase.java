package bank_access;

import mware_lib.Config;
import mware_lib.ObjectServerMessage;
import mware_lib.Skeleton;
import mware_lib.SkeletonFactory;

import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 28.11.13
 * Time: 23:12
 */
public  abstract class SkeletonFactoryBase implements SkeletonFactory {
    List<Skeleton> skeletons = new ArrayList<Skeleton>();
    Semaphore skeleton_per_servant = new Semaphore(Config.SKELETONS_PER_SERVANT,true);
    Class classTester;
    @Override
    public void removeSkeleton(Skeleton skeleton) {
        skeletons.remove(skeleton);
        skeleton_per_servant.release();
    }

    @Override
    public int getReferences() {
        return skeletons.size();
    }

    protected void addToSkeletonList(Skeleton skeleton) {
        try {
            System.out.println("permits:"+skeleton_per_servant.availablePermits());
            skeleton_per_servant.acquire();
            skeletons.add(skeleton);
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    protected void setClassTester(Object servant) {
          classTester = servant.getClass().getSuperclass();
    }
}
