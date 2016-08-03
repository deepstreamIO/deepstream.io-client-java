package io.deepstream;

/**
 * A listener that notifies the user whenever the record state is ready. Listeners can be added via
 * {@link List#addListReadyListener(ListReadyListener)} and removed via
 * {@link List#removeListReadyListener(ListReadyListener)}
 */
public interface ListReadyListener {
    /**
     * Called when the underlying record is ready
     * @param listName The name of the list which is now ready
     * @param list The list which is now ready / loaded from server
     */
    void onListReady(String listName, List list);
}