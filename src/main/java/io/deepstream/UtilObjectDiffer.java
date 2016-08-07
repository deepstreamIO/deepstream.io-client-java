package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import java.util.Map;


class UtilObjectDiffer {

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
     * The following would be returned in a Tuple:
     *
     * ( "favouriteCoffee.name", "Latte" )
     *
     * @param nodeA the old version of the object
     * @param nodeB the new version of the object
     * @return a Tuple with the path of the changes and the actual changes
     */
    public Tuple getUpdateObject(JsonElement nodeA, JsonElement nodeB) {
        return getDiff(nodeA, nodeB, new StringBuilder());
    }

    /**
     * Gets the difference of two objects given with the given path
     *
     * @param nodeA the old version of the object
     * @param nodeB the new version of the object
     * @param path current path of the changes
     * @return a Tuple with the path of the changes and the actual changes
     */
    public Tuple getUpdateObject(JsonElement nodeA, JsonElement nodeB, String path) {
        return getDiff( nodeA, nodeB, new StringBuilder(path) );
    }

    /**
     * Implementation that gets the actual diff object
     *
     * @param nodeA the old version of the object
     * @param nodeB the new version of the object
     * @param path the StringBuilder object that contains the current path
     * @return a Pair<String, JsonElement> with the path of the changes and the actual changes
     */
    private Tuple getDiff(JsonElement nodeA, JsonElement nodeB, StringBuilder path) {
        String nodePath = null;
        JsonElement diffNode = null;

        if( nodeA.equals(nodeB) ) {
            return new Tuple(path.toString(), null);
        }
        // Just return the whole array
        else if( nodeA instanceof JsonArray || nodeB instanceof JsonArray ) {
            return new Tuple(path.toString(), nodeB);
        }

        JsonObject node1 = (JsonObject) nodeA;
        JsonObject node2 = (JsonObject) nodeB;

        int difference = node1.entrySet().size() - node2.entrySet().size();
        if( difference != 0 ) {
            return new Tuple(path.toString(), node2);
        }

        boolean foundDifferentNode = false;
        for (Map.Entry<String, JsonElement> s : node1.entrySet()) {
            String key = s.getKey();

            // Missing path, send whole node
            if( node2.get(key) == null ) {
                return new Tuple(path.toString(), node2);
            }

            if( ! node2.get(key).equals(node1.get(key)) ) {
                // Found two attributes that are different
                // need to send whole node
                if( foundDifferentNode ) {
                    return new Tuple(path.toString(), node2);
                }

                diffNode = node2.get(key);
                nodePath = key;
                foundDifferentNode = true;
            }
        }
        // Only one attribute of node is different
        buildPath( nodePath, path );

        if( diffNode instanceof JsonObject || diffNode instanceof JsonArray ) {
            return getDiff( node1.get(nodePath), diffNode, path );
        }

        return new Tuple( path.toString(), getValue(diffNode) );
    }

    /**
     * Gets the primitive or string value of an element
     *
     * @param element the element to get the value from
     * @return the primitive or string value represented by the element
     */
    private Object getValue(JsonElement element) {
        JsonPrimitive primitive = element.getAsJsonPrimitive();
        if( primitive.isBoolean() ) {
            return primitive.getAsBoolean();
        } else if ( primitive.isNumber() ) {
            Number number = primitive.getAsNumber();
            double value = number.doubleValue();
            if ( (int) value == value ) {
                return number.intValue();
            }
            return value;
        } else {
            return primitive.getAsString();
        }
    }

    private void buildPath(String currentNodeName, StringBuilder path) {
        if( path.toString().equals("") ) {
            path.append( currentNodeName );
        } else {
            path.append( "." );
            path.append( currentNodeName );
        }
    }
}


/**
 * Internal class for returning path and value for update object
 */
class Tuple {

    final String path;
    final Object value;

    public Tuple(String path, Object value) {
        this.path = path;
        this.value = value;
    }
}