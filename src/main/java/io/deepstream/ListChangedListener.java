package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

/**
 * List change callback, occurs whenever an the entries change
 */

public interface ListChangedListener {
    /**
     * Notified whenever the entries in the list change
     * @param listName The name of list
     * @param entries A list containing all the record names
     */
    @ObjectiveCName("onListChanged:entries:")
    void onListChanged(String listName, java.util.List entries);
}
