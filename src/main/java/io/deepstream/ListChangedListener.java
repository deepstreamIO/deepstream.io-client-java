package io.deepstream;

public interface ListChangedListener {
    void onListChanged( String listname, java.util.List entries );
    void onEntryAdded( String listName, String entry, int position );
    void onEntryRemoved( String listName, String entry, int position );
    void onEntryMoved( String listName, String entry, int position );
}
