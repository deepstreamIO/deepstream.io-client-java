package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * An exception that is thrown if you try to do a {@link AnonymousRecord#discard()} or {@link AnonymousRecord#delete()}
 * before a record has been set via {@link AnonymousRecord#setName(String)}
 * </br>
 * The AnonymousRecordUninitialized extends {@link Exception}
 */
public class AnonymousRecordUninitialized extends Exception {
    /**
     * This Exception is thrown by {@see AnonymousRecord} and should not be constructed by consumers of this library
     * @param methodName The method name that was called
     */
    @ObjectiveCName("init:")
    AnonymousRecordUninitialized(String methodName) {
        super( "Can`t invoke " + methodName + ". AnonymousRecord not initialised. Call setName first" );
    }
}
