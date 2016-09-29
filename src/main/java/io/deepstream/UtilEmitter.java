package io.deepstream;

import com.google.j2objc.annotations.ObjectiveCName;

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
class UtilEmitter {

    private final ConcurrentMap<String, ConcurrentLinkedQueue<Object>> callbacks = new ConcurrentHashMap<>();

    private static boolean sameAs(Object fn, Object internal) {
        return fn.equals(internal) || internal instanceof OnceListener && fn.equals(((OnceListener) internal).fn);
    }

    /**
     * @param enu eventName as an Enum
     * @param fn The listener to invoke
     * @return a reference to this object.
     */
    @ObjectiveCName("on:fn:")
    public UtilEmitter on(Enum enu, Object fn ) {
        this.on( enu.toString(), fn );
        return this;
    }

    /**
     * Listens on the event.
     * @param event event name.
     * @param fn The listener to invoke
     * @return a reference to this object.
     */
    @ObjectiveCName("onWithEvent:fn:")
    public UtilEmitter on(String event, Object fn) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        if (callbacks == null) {
            callbacks = new ConcurrentLinkedQueue<>();
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
     * @param fn The listener to invoke
     * @return a reference to this object.
     */
    @ObjectiveCName("once:fn:")
    public UtilEmitter once(final String event, final Listener fn) {
        this.on(event, new OnceListener(event, fn));
        return this;
    }

    /**
     * Removes all registered listeners.
     *
     * @return a reference to this object.
     */
    public UtilEmitter off() {
        this.callbacks.clear();
        return this;
    }

    /**
     * Removes all listeners of the specified event.
     *
     * @param event an event name.
     * @return a reference to this object.
     */
    @ObjectiveCName("off:")
    public UtilEmitter off(String event) {
        this.callbacks.remove(event);
        return this;
    }

    /**
     * Removes the listener.
     *
     * @param event an event name.
     * @param fn The listener to invoke
     * @return a reference to this object.
     */
    @ObjectiveCName("off:fn:")
    public UtilEmitter off(String event, Object fn) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        if (callbacks != null) {
            Iterator<Object> it = callbacks.iterator();
            while (it.hasNext()) {
                Object internal = it.next();
                if (UtilEmitter.sameAs(fn, internal)) {
                    it.remove();
                    break;
                }
            }
        }
        return this;
    }

    /**
     * Returns a list of listeners for the specified event.
     *
     * @param event an event name.
     * @return a reference to this object.
     */
    @ObjectiveCName("listeners:")
    public List<Object> listeners(String event) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        return callbacks != null ?
                new ArrayList<>(callbacks) : new ArrayList<>(0);
    }

    /**
     * Check if this emitter has listeners for the specified event.
     *
     * @param event an event name.
     * @return true if a listener exists for that eventname
     */
    @ObjectiveCName("hasListeners:")
    public boolean hasListeners(String event) {
        ConcurrentLinkedQueue<Object> callbacks = this.callbacks.get(event);
        return callbacks == null || callbacks.isEmpty();
    }

    /**
     * Check if this emitter has any listeners
     * @return all listeners
     */
    public Set<String> getEvents() {
        return this.callbacks.keySet();
    }

    /**
     * Check if this emitter has any listeners
     *
     * @return true if any listeners exist
     */
    public boolean hasListeners() {
        return this.callbacks.isEmpty();
    }

    interface Listener {
        @ObjectiveCName("call:")
        void call(Object... args);
    }

    private class OnceListener implements Listener {

        public final String event;
        public final Listener fn;

        @ObjectiveCName("init:fn:")
        public OnceListener(String event, Listener fn) {
            this.event = event;
            this.fn = fn;
        }

        @Override
        @ObjectiveCName("call:")
        public void call(Object... args) {
            UtilEmitter.this.off(this.event, this);
            this.fn.call(args);
        }
    }
}