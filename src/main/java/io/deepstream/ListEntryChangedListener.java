package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * List entry callbacks, called whenever an entry is added, removed or moved within the list
 */
public interface ListEntryChangedListener {
    /**
     * Notified whenever an entry is added
     * @param listName The name of the list
     * @param entry The name of the record that was added to the list
     * @param position The index the recordName was added to
     */
    @ObjectiveCName("onEntryAdded:entry:position:")
    void onEntryAdded(String listName, String entry, int position);

    /**
     * Notified whenever an entry is removed
     * @param listName The name of the list
     * @param entry The name of the record that was removed to the list
     * @param position The index the recordName was removed from
     */
    @ObjectiveCName("onEntryRemoved:entry:position:")
    void onEntryRemoved(String listName, String entry, int position);

    /**
     * Notified whenever an entry is added
     * @param listName The name of the list
     * @param entry The name of the record that was moved within the list
     * @param position The index the recordName was moved to
     */
    @ObjectiveCName("onEntryMoved:entry:position:")
    void onEntryMoved(String listName, String entry, int position);
}
