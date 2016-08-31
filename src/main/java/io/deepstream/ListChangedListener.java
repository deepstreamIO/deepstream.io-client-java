package io.deepstream;

/**
 * List change callback, occurs whenever an the entries change
 */
public interface ListChangedListener {
    /**
     * Notified whenever the entries in the list change
     * @param listName The name of list
     * @param entries A list containing all the record names
     */
    void onListChanged(String listName, java.util.List entries);
}
