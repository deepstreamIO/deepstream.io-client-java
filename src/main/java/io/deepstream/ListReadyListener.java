package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * A listener that notifies the user whenever the list state is ready.
 */
@ObjectiveCName("ListReadyListener")
public interface ListReadyListener {
    /**
     * Called when the list is loaded from the server
     *
     * @param listName The name of the list which is now ready
     * @param list     The list which is now ready / loaded from server
     */
    @ObjectiveCName("onListReady:list:")
    void onListReady(String listName, List list);
}
