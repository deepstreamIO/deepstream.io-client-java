package io.deepstream;

/**
 * List change callbacks
 * TODO: Split these for performance reasons?
 */
public interface ListChangedListener {
    /**
     * Notified whenever the entries in the list change
     * @param listname The name of list
     * @param entries A list containing all the record names
     */
    void onListChanged( String listname, java.util.List entries );

    /**
     * Notified whenever an entry is added
     * @param listName The name of the list
     * @param entry The name of the record that was added to the list
     * @param position The index the recordName was added to
     */
    void onEntryAdded( String listName, String entry, int position );

    /**
     * Notified whenever an entry is removed
     * @param listName The name of the list
     * @param entry The name of the record that was removed to the list
     * @param position The index the recordName was removed from
     */
    void onEntryRemoved( String listName, String entry, int position );

    /**
     * Notified whenever an entry is added
     * @param listName The name of the list
     * @param entry The name of the record that was moved within the list
     * @param position The index the recordName was moved to
     */
    void onEntryMoved( String listName, String entry, int position );
}
