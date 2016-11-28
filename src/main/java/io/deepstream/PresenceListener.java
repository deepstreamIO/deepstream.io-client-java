package io.deepstream;


import java.util.ArrayList;

public interface PresenceListener {
    void onClients(ArrayList<String> names);
}
