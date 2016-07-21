package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import javafx.util.Pair;

import java.util.Map;


class UtilObjectDiffer {

    StringBuilder path;
    JsonElement value;
    String key;
    String pathKey;

    public Pair<String, JsonElement> getUpdateObject(JsonElement nodeA, JsonElement nodeB) {
        //Hack to clear state
        path = new StringBuilder();
        value = null;
        key = null;
        pathKey = null;
        return getDiff(nodeA, nodeB);
    }

    /**
     * Gets the difference between two objects.
     *
     * Given the json representation of two POJOs:
     *
     * old = {
     *     name: "Marge",
     *     age: 30,
     *     favouriteCoffee: {
     *         name: "Flat White",
     *         price: 6
     *     }
     * }
     *
     * new = {
     *     name: "Marge",
     *     age: 30,
     *     favouriteCoffee: {
     *         name: "Latte",
     *         price: 6
     *     }
     * }
     *
     * The following would be returned in a Pair:
     *
     * ( "favouriteCoffee.name", "Latte" )
     *
     * @param nodeA the old version of the object
     * @param nodeB the new version of the object
     * @return a Pair<String,Object> with the path of the changes and the actual changes
     */
    private Pair<String, JsonElement> getDiff(JsonElement nodeA, JsonElement nodeB) {

        if( nodeA.equals(nodeB) ) {
            System.out.println("Same object, returning");
            return new Pair(path.toString(), null);
        }
        else if( nodeA instanceof JsonArray || nodeB instanceof JsonArray ) {
            System.out.println("Array, returning whole new node");
            return new Pair(path.toString(), nodeB);
        }

        JsonObject node1 = (JsonObject) nodeA;
        JsonObject node2 = (JsonObject) nodeB;

        int difference = node1.entrySet().size() - node2.entrySet().size();
        if( difference != 0 ) {
            System.out.println("Size of objects are different, need to update whole node");
            return new Pair(path.toString(), node2);
        }

        boolean foundDifferentNode = false;

        // Compare attributes of JsonElement
        for (Map.Entry<String, JsonElement> s : node1.entrySet()) {
            key = s.getKey();

            if( node2.get(key) == null ) {
                System.out.println("A path is missing, send whole new path");
                return new Pair(path.toString(), node2);
            }

            if( node2.get(key).equals(node1.get(key)) ) {
                System.out.printf("Same values old:%s new:%s\n", node1.get(key), node2.get(key));
            }

            else {

                if( foundDifferentNode ) {
                    System.out.println("Found two different! BREAK");
                    return new Pair(path.toString(), node2);
                }

                System.out.printf("Found a different value old:%s new:%s\n", node1.get(key), node2.get(key));
                value = node2.get(key);
                pathKey = key;
                foundDifferentNode = true;
            }
        }

        buildPath( pathKey );

        if( value instanceof JsonObject ) {
            System.out.println("Going deeper");
            return getDiff( node1.get(key), value);
        }
        return new Pair(path.toString(), value);
    }

    private void buildPath(String currentNodeName) {
        if( path.toString().equals("") ) {
            path.append( currentNodeName );
        } else {
            path.append( "." );
            path.append( currentNodeName );
        }
    }
}