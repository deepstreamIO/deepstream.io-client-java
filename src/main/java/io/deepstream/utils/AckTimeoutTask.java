package io.deepstream.utils;

import java.util.TimerTask;

class AckTimeoutTask extends TimerTask {
    private String name;
    private AckTimeoutCallback callback;

    AckTimeoutTask( String name, AckTimeoutCallback callback ) {
        this.name = name;
        this.callback = callback;
    }

    @Override
    public void run() {
        this.callback.onTimeout( this.name );
    }
}