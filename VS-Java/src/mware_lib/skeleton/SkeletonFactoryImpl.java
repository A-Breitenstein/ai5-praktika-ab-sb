package mware_lib.skeleton;

import mware_lib.Config;
import mware_lib.object_server.ObjectServerMessage;

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
public class SkeletonFactoryImpl implements SkeletonFactory {
    Object servant;

    List<Skeleton> skeletons = new ArrayList<Skeleton>();
    Semaphore skeleton_per_servant = new Semaphore(Config.SKELETONS_PER_SERVANT,true);

    public SkeletonFactoryImpl(Object servant) {
        this.servant = servant;
    }

    @Override
    public Skeleton createSkeleton(Socket clientSocket, ObjectServerMessage serviceMessage) {
        Skeleton tmp = new SkeletonImpl(this,servant,serviceMessage,getReferences(),clientSocket );
        addToSkeletonList(tmp);
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
    private void addToSkeletonList(Skeleton skeleton) {
        try {
            if(Config.DEBUG) System.out.println("permits:"+skeleton_per_servant.availablePermits());
            skeleton_per_servant.acquire();
            skeletons.add(skeleton);
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    @Override
    public String toString() {
        return "SkeletonFactoryImpl{" +
                "servant=" + servant +
                '}';
    }
}
