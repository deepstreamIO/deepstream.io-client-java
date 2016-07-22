package io.deepstream;


import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;


/**
 * The event emitter which is ported from the JavaScript module. This class is thread-safe.
 *
 * @see <a href="https://github.com/component/emitter">https://github.com/component/emitter</a>
 */
class Emitter {

    private ConcurrentMap<String, ConcurrentLinkedQueue<Object>> callbacks
            = new ConcurrentHashMap<String, ConcurrentLinkedQueue<Object>>();

    /**
     *
     * @param enu eventName as an Enum
     * @param fn
     * @return a reference to this object.
     */
    public Emitter on( Enum enu, Object fn ) {
        this.on( enu.toString(), fn );
        return this;
    }

    /**
     * Listens on the event.
     * @param event event name.
     * @param fn
     * @return a reference to this object.
     */
    public Emitter on(String event, Object fn) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        if (callbacks == null) {
            callbacks = new ConcurrentLinkedQueue <Object>();
            ConcurrentLinkedQueue<Object> _callbacks = this.callbacks.putIfAbsent(event, callbacks);
            if (_callbacks != null) {
                callbacks = _callbacks;
            }
        }
        callbacks.add(fn);
        return this;
    }

    /**
     * Adds a one time listener for the event.
     *
     * @param event an event name.
     * @param fn
     * @return a reference to this object.
     */
    public Emitter once(final String event, final Listener fn) {
        this.on(event, new OnceListener(event, fn));
        return this;
    }

    /**
     * Removes all registered listeners.
     *
     * @return a reference to this object.
     */
    public Emitter off() {
        this.callbacks.clear();
        return this;
    }

    /**
     * Removes all listeners of the specified event.
     *
     * @param event an event name.
     * @return a reference to this object.
     */
    public Emitter off(String event) {
        this.callbacks.remove(event);
        return this;
    }

    /**
     * Removes the listener.
     *
     * @param event an event name.
     * @param fn
     * @return a reference to this object.
     */
    public Emitter off(String event, Object fn) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        if (callbacks != null) {
            Iterator<Object> it = callbacks.iterator();
            while (it.hasNext()) {
                Object internal = it.next();
                if (Emitter.sameAs(fn, internal)) {
                    it.remove();
                    break;
                }
            }
        }
        return this;
    }

    private static boolean sameAs(Object fn, Object internal) {
        if (fn.equals(internal)) {
            return true;
        } else if (internal instanceof OnceListener) {
            return fn.equals(((OnceListener) internal).fn);
        } else {
            return false;
        }
    }

    /**
     * Returns a list of listeners for the specified event.
     *
     * @param event an event name.
     * @return a reference to this object.
     */
    public List<Object> listeners(String event) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        return callbacks != null ?
                new ArrayList<Object>(callbacks) : new ArrayList<Object>(0);
    }

    /**
     * Check if this emitter has listeners for the specified event.
     *
     * @param event an event name.
     * @return
     */
    public boolean hasListeners(String event) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        return callbacks != null && !callbacks.isEmpty();
    }

    /**
     * Check if this emitter has any listeners
     *
     * @return
     */
    public Set getEvents() {
        return this.callbacks.keySet();
    }

    /**
     * Check if this emitter has any listeners
     *
     * @return
     */
    public boolean hasListeners() {
        return this.callbacks.isEmpty();
    }

    public static interface Listener {
        public void call(Object... args);
    }

    private class OnceListener implements Listener {

        public final String event;
        public final Listener fn;

        public OnceListener(String event, Listener fn) {
            this.event = event;
            this.fn = fn;
        }

        @Override
        public void call(Object... args) {
            Emitter.this.off(this.event, this);
            this.fn.call(args);
        }
    }
}