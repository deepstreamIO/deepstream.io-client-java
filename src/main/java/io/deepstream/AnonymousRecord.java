package io.deepstream;

import java.util.ArrayList;

/**
 * An AnonymousRecord is a record without a predefined name. It
 * acts like a wrapper around an actual record that can
 * be swapped out for another one whilst keeping all bindings intact.
 *
 * Imagine a customer relationship management system with a list of users
 * on the left and a user detail panel on the right. The user detail
 * panel could use the anonymous record to set up its bindings, yet whenever
 * a user is chosen from the list of existing users the anonymous record's
 * setName method is called and the detail panel will update to
 * show the selected user's details
 */
class AnonymousRecord  {

    private Record record;
    private final ArrayList subscriptions;

    /**
     * @param recordHandler
     */
    public AnonymousRecord(RecordHandler recordHandler) {
        subscriptions = new ArrayList<Subscription>();
    }

    /**
     * Proxies the actual record's get method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @return
     */
    public Object get() {
        if( record == null ) {
            return null;
        }
        return record.get();
    }

    /**
     * Proxies the actual record's get method. It is valid
     * to call get prior to setName - if no record exists,
     * the method returns null
     * @param path
     * @return
     */
    public Object get( String path ) {
        if( record == null ) {
            return null;
        }
        return record.get( path );
    }

    /**
     *
     * @param recordChangedCallback
     * @param triggerNow
     * @return
     */
    public AnonymousRecord subscribe( RecordChangedCallback recordChangedCallback, boolean triggerNow ) {
        return this.subscribe( null, recordChangedCallback, triggerNow );
    }

    /**
     *
     * @param recordChangedCallback
     * @return
     */
    public AnonymousRecord subscribe( RecordChangedCallback recordChangedCallback ) {
        return this.subscribe( null, recordChangedCallback, false );
    }

    /**
     *
     * @param path
     * @param recordChangedCallback
     * @return
     */
    public AnonymousRecord subscribe( String path, RecordChangedCallback recordChangedCallback ) {
        return this.subscribe( path, recordChangedCallback, false );
    }

    /**
     * Proxies the actual record's subscribe method. The same parameters
     * can be used. Can be called prior to setName(). Please note, triggerIfReady
     * will always be set to true to reflect changes in the underlying record.
     * @param path
     * @param recordChangedCallback
     * @param triggerNow
     * @return
     */
    public AnonymousRecord subscribe( String path, RecordChangedCallback recordChangedCallback, boolean triggerNow ) {
        this.subscriptions.add( new Subscription( path, recordChangedCallback ) );

        if( record != null ) {
            record.subscribe( path, recordChangedCallback );
        }

        return this;
    }



    private class Subscription {
        String path;
        RecordChangedCallback recordChangedCallback;
        Subscription(String path, RecordChangedCallback recordChangedCallback ) {
            this.path = path;
            this.recordChangedCallback = recordChangedCallback;
        }
    }
}
