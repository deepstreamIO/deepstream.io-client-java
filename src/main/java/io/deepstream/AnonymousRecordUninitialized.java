package io.deepstream;

/**
 * An exception that is thrown if you try to do a {@link AnonymousRecord#discard()} or {@link AnonymousRecord#delete()}
 * before a record has been set via {@link AnonymousRecord#setName(String)}
 */
public class AnonymousRecordUninitialized extends Exception {
    /**
     * This Exception is thrown by {@see AnonymousRecord} and should not be constructed by consumers of this library
     * @param methodName The method name that was called
     */
    AnonymousRecordUninitialized(String methodName) {
        super( "Can`t invoke " + methodName + ". AnonymousRecord not initialised. Call setName first" );
    }
}
