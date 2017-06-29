package io.deepstream;

import com.google.gson.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Objects;

class UtilJSONPath {

    private JsonElement coreElement;

    public UtilJSONPath(JsonElement e){
        this.coreElement = e;
    }

    /**
     * Traverses through the json element tree for retrieving values
     *
     * @param element The element to traverse through
     * @param path The path to find
     * @return The element indicated by the path
     */
    private static JsonElement getIterateThrough(JsonElement element, String path) {
        ArrayList<Object> tokens = tokenize(path);
        JsonElement traverser = element;
        Object token = null;

        for( int i=0; i < tokens.size(); i++ ) {
            token = tokens.get(i);

            try {
                if (traverser == null || traverser.isJsonNull()) {
                    break;
                }

                if (traverser.isJsonObject()) {
                    traverser = traverser.getAsJsonObject().get((String) token);
                } else if (traverser.isJsonArray()) {
                    traverser = traverser.getAsJsonArray().get((int) token);
                } else {
                    traverser = null;
                }
            } catch( IndexOutOfBoundsException e ) {
                return null;
            }
        }
        return traverser;
    }

    private static ArrayList<Object> tokenize(String path) {
        ArrayList<Object> tokens = new ArrayList<>();
        String[] parts = path.split("\\.");

        for (int i = 0; i < parts.length; i++) {
            String part = parts[i];

            if (part.length() == 0) {
                continue;
            }

            String[] arrayIndexes = part.split("[\\[\\]]");
            tokens.add(arrayIndexes[0].trim());

            for (int j = 1; j < arrayIndexes.length; j++) {
                if (arrayIndexes[j].length() == 0) {
                    continue;
                }
                tokens.add(Integer.valueOf(arrayIndexes[j].trim()));
            }
        }
        return tokens;
    }

    private static JsonElement setIterateThrough (JsonElement element, String path, JsonElement value, boolean delete) {

        ArrayList<Object> tokens = tokenize(path);
        JsonElement[] nodes = new JsonElement[tokens.size()];
        nodes[0] = element;
        JsonElement traverser = null;

        // will be either a String or int
        Object token = null;
        int i;
        for(i = 0; i < tokens.size(); i++) {
            JsonElement parent = nodes[i];
            token = tokens.get(i);

            Object nextToken;
            try {
                nextToken = tokens.get(i + 1);
            } catch (IndexOutOfBoundsException e) {
                break;
            }

            if (parent.isJsonObject()) {
                JsonElement child = parent.getAsJsonObject().get((String) token);
                if (child != null) {
                    if (nextToken instanceof String && child.isJsonObject())
                        traverser = parent.getAsJsonObject().get((String) token);
                    else if (nextToken instanceof String && (!child.isJsonObject())) {
                        parent.getAsJsonObject().add((String) token, new JsonObject());
                        traverser = parent.getAsJsonObject().get((String) token);
                    } else if (nextToken instanceof Integer && child.isJsonArray()) {
                        traverser = parent.getAsJsonObject().get((String) token);
                    } else if (nextToken instanceof Integer && !child.isJsonArray()) {
                        parent.getAsJsonObject().add((String) token, initialiseArray((int) nextToken));
                        traverser = parent.getAsJsonObject().get((String) token);
                    }
                } else {
                    if (nextToken instanceof Integer) {
                        parent.getAsJsonObject().add((String) token, initialiseArray((int) nextToken));
                        traverser = parent.getAsJsonObject().get((String) token);
                    } else {
                        parent.getAsJsonObject().add((String) token, new JsonObject());
                        traverser = parent.getAsJsonObject().get((String) token);
                    }
                }
            } else if (parent.isJsonArray()) {
                JsonElement child = null;
                try {
                    parent.getAsJsonArray().get((int) token);
                } catch (IndexOutOfBoundsException e) {
                    extendArray(parent.getAsJsonArray(), (int) token);

                    if (nextToken instanceof Integer) {
                        JsonArray array = initialiseArray((int) nextToken);
                        parent.getAsJsonArray().set((int) token, array);
                        traverser = parent.getAsJsonArray().get((int) token);
                    } else {
                        parent.getAsJsonArray().set((int) token, new JsonObject());
                        traverser = parent.getAsJsonArray().get((int) token);
                    }
                }

                child = parent.getAsJsonArray().get((int) token);

                if (child != null) {
                    if (nextToken instanceof String && child.isJsonObject())
                        traverser = parent.getAsJsonArray().get((int) token);
                    else if (nextToken instanceof String && (!child.isJsonObject())) {
                        parent.getAsJsonArray().set((int) token, new JsonObject());
                        traverser = parent.getAsJsonArray().get((int) token);
                    } else if (nextToken instanceof Integer && child.isJsonArray()) {
                        traverser = parent.getAsJsonArray().get((int) token);
                    } else if (nextToken instanceof Integer && !child.isJsonArray()) {
                        parent.getAsJsonArray().set((int) token, initialiseArray((int) nextToken));
                        traverser = parent.getAsJsonArray().get((int) token);
                    }
                } else {
                    if (nextToken instanceof Integer) {
                        parent.getAsJsonObject().add((String) token, initialiseArray((int) nextToken));
                        traverser = parent.getAsJsonObject().get((String) token);
                    } else {
                        parent.getAsJsonObject().add((String) token, new JsonObject());
                        traverser = parent.getAsJsonObject().get((String) token);
                    }
                }

                if (traverser.isJsonNull()) {
                    if (nextToken instanceof Integer) {
                        JsonArray arr = initialiseArray((int) nextToken);
                        parent.getAsJsonArray().set((int) token, arr);
                    } else if (nextToken instanceof String) {
                        parent.getAsJsonArray().set((int) token, new JsonObject());
                    }
                    traverser = parent.getAsJsonArray().get((int) token);
                }

                if (traverser.isJsonArray()) {
                    if (nextToken instanceof Integer && traverser.getAsJsonArray().size() < (int) nextToken) {
                        extendArray(traverser.getAsJsonArray(), (int) nextToken);
                    }
                }
            }
            nodes[i + 1] = traverser;
        }
        if( token != null && (value != null || delete) ) {
            updateValue(value, nodes[nodes.length - 1], token, delete);
        }
        return traverser;
    }

    private static JsonArray initialiseArray(int size) {
        JsonArray array = new JsonArray();
        for (int j = 0; j < size + 1; j++) {
            array.add(JsonNull.INSTANCE);
        }
        return array;
    }

    private static void extendArray(JsonArray array, int size) {
        for (int j = array.size(); j < size + 1; j++) {
            array.add(JsonNull.INSTANCE);
        }
    }

    private static void updateValue(JsonElement value, JsonElement parent, Object token, boolean delete) {
        if( parent.isJsonObject() ) {
            JsonObject object = (JsonObject) parent;
            if( delete ) {
                object.remove((String) token);
            } else {
                object.add((String) token, value );
            }
        }
        else if( parent.isJsonArray() ) {
            JsonArray object = (JsonArray) parent;
            int size = object.size();

            if( delete ) {
                object.remove((int) token);
            } else {
                for( int i=size; i<= (int) token; i++ ){
                    object.add( JsonNull.INSTANCE );
                }
                object.set((int) token, value);
            }
        }
    }

    private static JsonElement getArrayElement(JsonElement traverser,
                                               String token) {

        int index =  Integer.valueOf( getIndex(token) );
        try {
            return traverser.getAsJsonObject()
                    .get(getTokenPrefix(token)).getAsJsonArray()
                    .get(index);
        } catch( ArrayIndexOutOfBoundsException e ) {
            return null;
        } catch( IndexOutOfBoundsException e ) {
            return null;
        }
    }

    private static String getTokenPrefix(String token) {
        return token.substring( 0, token.indexOf( "[" ) );
    }

    private static String getIndex(String token) {
        return token.substring(token.indexOf("[") + 1, token.indexOf("]")).trim();
    }

    private static boolean isArray(String token) {
        boolean isArray = ( token.contains("[") && token.contains("]") && (token.indexOf("[") < token.indexOf("]")));
        try {
            Integer.parseInt( token.substring(token.indexOf("[")+1, token.indexOf("]") ).trim() );
            return isArray;
        } catch (Exception e) {
            return false;
        }
    }

    public JsonElement get(String path) {
        if (Objects.equals(path, "") || path == null) {
            return this.coreElement;
        } else {
            return getIterateThrough(this.coreElement, path);
        }
    }

    public void set(String path, JsonElement value) {
        if (Objects.equals(path, "")) {
            throw new RuntimeException("Setting an entire object must be done via setValue( JsonElement value );");
        } else if (path == null) {
            this.coreElement = value;
        } else {
            setIterateThrough(this.coreElement, path, value, false);
        }
    }

    /**
     * Deletes the value specified in the path.
     *
     * Traverses through the tree to find the parent object and then
     * just removes the key
     *
     * @param path The path to delete
     */
    protected void delete(String path) {
        setIterateThrough(this.coreElement, path, null, true);
    }

    public JsonElement getCoreElement() {
        return coreElement;
    }

    public void setCoreElement(JsonElement coreElement) {
        this.coreElement = coreElement;
    }

    public class Array implements Iterable<UtilJSONPath> {
        private final JsonArray root;
        private JsonElement coreElement;

        public Array(JsonArray root) {
            this.root = root;
        }

        @Override
        public Iterator<UtilJSONPath> iterator() {
            return new Iterator<UtilJSONPath>(){
                final Iterator<JsonElement> it = root.iterator();
                @Override
                public boolean hasNext() {
                    return it.hasNext();
                }
                @Override
                public UtilJSONPath next() {
                    return new UtilJSONPath(it.next());
                }
                @Override
                public void remove() {
                    it.remove();
                }};
        }
    }
}