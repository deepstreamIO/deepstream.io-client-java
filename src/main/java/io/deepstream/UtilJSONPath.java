package io.deepstream;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;

import java.util.Iterator;

class UtilJSONPath {
    private JsonElement coreElement;

    public UtilJSONPath(JsonElement e){
        this.coreElement = e;
    }

    public JsonElement get(String path){
        if( path == "" || path == null ){
            return this.coreElement;
        } else {
            return iterateThrough(this.coreElement, path, null);
        }
    }

    public void set( String path, JsonElement value ) {
        if( path == "" ) {
            throw new RuntimeException( "Setting an entire object must be done via setValue( JsonElement value );" );
        } else if( path == null ) {
           this.coreElement = value;
        } else {
            iterateThrough(this.coreElement, path, value);
        }
    }

    private static JsonElement iterateThrough (JsonElement element, String path, JsonElement value) {
        String[] st = path.split( "\\." );
        System.out.println( st.length );
        JsonElement parent = null;
        JsonElement traverser = element;
        String token = null;

        for( int i=0; i<st.length; i++ ) {
            token = st[ i ];

            parent = traverser;

            System.out.println( token );

            try {
                if (isArray(token)) {
                    traverser = getArrayElement(traverser, token);
                    token = getIndex( token );
                } else if( traverser.isJsonObject() ){
                    traverser = traverser.getAsJsonObject().get(token);
                } else if( traverser.isJsonArray() ){
                    break;
                }
            } catch( NullPointerException e ) {
                if( value != null ) {
                    if (isArray(token)) {
                        int index = Integer.parseInt(getIndex(token));
                        String prefix = getTokenPrefix(token);
                        JsonArray array = new JsonArray();

                        for (int j = 0; j < index; j++) {
                            array.add(JsonNull.INSTANCE);
                        }

                        if (i!=st.length) {
                            JsonElement temp = new JsonObject();
                            array.add(temp);
                        }

                        traverser.getAsJsonObject().add(prefix, array);
                        traverser = array.get(index);
                    }
                }
            }
        }

        if( value != null && token != null ) {
            updateValue(value, parent, token);
        }

        return traverser;
    }

    private static void updateValue(JsonElement value, JsonElement parent, String token) {
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

    public void setCoreElement(JsonElement coreElement) {
        this.coreElement = coreElement;
    }

    public JsonElement getCoreElement() {
        return coreElement;
    }

    public class Array implements Iterable<UtilJSONPath> {
        private JsonArray root;
        private JsonElement coreElement;

        public Array(JsonArray root) {
            this.root = root;
        }

        @Override
        public Iterator<UtilJSONPath> iterator() {
            return new Iterator<UtilJSONPath>(){
                Iterator<JsonElement> it = root.iterator();
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