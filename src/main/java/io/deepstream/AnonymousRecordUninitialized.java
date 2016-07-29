package io.deepstream;

public class AnonymousRecordUninitialized extends Throwable {
    public AnonymousRecordUninitialized(String methodName) {
        super( "Can`t invoke " + methodName + ". AnonymousRecord not initialised. Call setName first" );
    }
}
