package mware_lib.stub;

import name_service.NameServiceMessage;

/**
 * Created with IntelliJ IDEA.
 * User: Sven
 * Date: 05.12.13
 * Time: 19:43
 */
public class StubFactory {

    public static Stub createStub(NameServiceMessage nameServiceMessage) {
        return new StubImpl(nameServiceMessage);
//        return new StubImplReuseConnection(nameServiceMessage);
    }
}
