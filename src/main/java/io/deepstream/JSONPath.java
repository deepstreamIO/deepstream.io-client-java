package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;

import java.util.Iterator;
import java.util.StringTokenizer;

public class JSONPath {
    private JsonElement coreElement;

    public JSONPath(JsonElement e){
        this.coreElement = e;
    }

    public JsonElement get(String path){
        if( path == "" ){
            return this.coreElement;
        }
        return iterateThrough(this.coreElement, path, null);
    }

    public void set( String path, JsonElement value ) {
        if( path == "" ) {
            throw new RuntimeException( "Setting an entire object must be done via setValue( JsonElement value );" );
        }
        iterateThrough(this.coreElement, path, value);
    }

    private static JsonElement iterateThrough (JsonElement element, String path, JsonElement value) {
        StringTokenizer st = new StringTokenizer(path, ".");
        JsonElement parent = null;
        JsonElement traverser = element;
        String token = null;

        while (st.hasMoreTokens() && !traverser.isJsonNull()) {
            token = st.nextToken();
            parent = traverser;
            if (isArray(token)) {
                traverser = getArrayElement(element, token);
                if (traverser.isJsonArray() ) {
                    parent = traverser;
                } else {
                    parent = traverser.getAsJsonObject().get( getTokenPrefix( token ) );
                }
                token = getIndex( token );
            } else {
                traverser = traverser.getAsJsonObject().get(token);
            }
        }

        if( value != null && token != null ) {
            if( parent.isJsonObject() ) {
                JsonObject object = (JsonObject) parent;
                object.add( token, value );
            }
            else if( parent.isJsonArray() ) {
                JsonArray object = (JsonArray) parent;
                int size = object.size();
                int index = Integer.parseInt( token );
                for( int i=size; i<=index; i++ ){
                    object.add( JsonNull.INSTANCE );
                }
                object.set( index, value );
            }
        }

        return traverser;
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
        } catch( NullPointerException e ) {
            String arrayName = getTokenPrefix(token);
            JsonObject object = new JsonObject();
            object.add( arrayName, new JsonArray() );
            return object;
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

    public Array getIterable(String path) {
        JsonElement j = this.get(path);
        return j.isJsonArray()? new Array(j.getAsJsonArray()) : null;
    }

    public class Array implements Iterable<JSONPath> {
        private JsonArray root;

        public Array(JsonArray root) {
            this.root = root;
        }

        @Override
        public Iterator<JSONPath> iterator() {
            return new Iterator<JSONPath>(){
                Iterator<JsonElement> it = root.iterator();
                @Override
                public boolean hasNext() {
                    return it.hasNext();
                }
                @Override
                public JSONPath next() {
                    return new JSONPath(it.next());
                }
                @Override
                public void remove() {
                    it.remove();
                }};
        }
    }
}