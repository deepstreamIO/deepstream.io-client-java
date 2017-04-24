package io.deepstream;

import com.google.gson.*;

import java.util.Iterator;
import java.util.Objects;

class UtilJSONPath {
    private JsonElement coreElement;
    private Gson gson = new Gson();

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

        if (path == null || path.trim().isEmpty() ) {
            return element;
        }

        String p = path.replaceAll("\\s","");
        p = p.replaceAll("\\[(.*?)\\]", ".=$1");
        String[] pathTokens = p.split( "\\." );

        // check and if necessary init the root/parent JsonElement type
        // if first token is array index (e.g. [1] parsed to ".=1")
        // then return without update because
        // DS records must be a root JsonObject,
        // so do NOT support paths that expect root level to be a JsonArray,
        // root must be JsonObject
        //
        // also if first token is not an array index and the root element
        // is not already defined to be a JsonObject this is an
        // error condition too, just return without updating
        if ( pathTokens[0].trim().isEmpty() ) {
            // DS records must be a root JsonObject, so do NOT support paths that expect root level to be a JsonArray, root must be JsonObject
            return null;
        } else if ( !element.isJsonObject() ) {
            return null;
        }

        int index = -1;
        JsonElement traverser = element;
        String token = null;
        int i = 0;

        for( i = 0; i < pathTokens.length; i++ ) {

            token = pathTokens[ i ];

            // tokens can only be either
            // 1) array index elements
            // 2) object members

            if ( traverser == null || traverser.isJsonNull() ) {
                return null;
            }

            try {
                index = getArrayToken( token  );
                Boolean isArrayToken = ( index != -1 ) ? true : false;

                if ( isArrayToken ) {

                    if ( !traverser.isJsonArray() ) {
                        return null;
                    }
                    traverser = traverser.getAsJsonArray().get( index );

                } else if ( traverser.isJsonObject() ) {

                    traverser = traverser.getAsJsonObject().get( token );

                } else if ( traverser.isJsonArray() ) {

                    // TODO: @honorcode
                    // TODO: err condition -> have a token that is not an arrayToken and the current element to traverse is an JsonArray - mismatch, bad path ?
                    // TODO: @honorcode - err? Shouldn't happen unless bad access path
                    // TODO: that doesn't match the model, err?
                    return null;

                } else {

                    // TODO: @honorcode - this code branch should NOT be a reachable condition anymore but if not (bug?) ..... do what?
                    // TODO: @honorcode - err handler ?, logger ?, throw exception ?
                    return null; // TODO: @honorcode - remove 'else' ????
                }

            } catch ( Exception e ) {

                return null;
            }
        }

        return traverser;
    }

    /* sync'd logic with that in deepstream.io server source json-path.js

      // makes json path array items a single 'part' value of parts below
      // 'arrayProp[#]' members transform to 'arrayProp=#' now instead of 'arrayProp.#' previously
      // see setValue fnc above for special handling of array item parsing vs numeric obj member name
      // e.g. 'object.1' parsing. this allows for support of parsing and differentiating object
      // member names that are also numeric values
      // also supports multi-dimensional arrays e.g. arr[0][1][2][3]... => arr.=0.=1.=2.=3...
      // note: array index tokens are prefixed from regex with a '=' e.g. .=0.=1.=2 compared with
      // numeric obj field names tokens which are just .0.1.2.3
      let str = this._path.replace(/\s/g, '')
      str = str.replace(/\[(.*?)\]/g, '.=$1')
      const SPLIT_REG_EXP = /[.[\]]/g
      const parts = str.split(SPLIT_REG_EXP)

    */

    // @honorcode
    // the intent in the logic updates below and above in getIterateThrough
    // is to consistently support json path tokens having
    // numeric object member names (e.g. abc.1.xyz.4) and
    // multi-dim arrays (e.g. array[0][1][2][3])
    // with regex parsing and rules that match the
    // deepstream.io server json-path.js parser logic

    private static void setIterateThrough (JsonElement element, String path, JsonElement value, boolean delete) {

        if (path == null || path.trim().isEmpty() ) {
            return;
        }

        String p = path.replaceAll("\\s","");
        p = p.replaceAll("\\[(.*?)\\]", ".=$1");
        String[] pathTokens = p.split( "\\." );

        // check and if necessary init the root/parent JsonElement type
        // if first token is array index (e.g. [1] parsed to ".=1")
        // then return without update because
        // DS records must be a root JsonObject,
        // so do NOT support paths that expect root level to be a JsonArray,
        // root must be JsonObject
        //
        // also if first token is not an array index and the root element
        // is not already defined to be a JsonObject this is an
        // error condition too, just return without updating
        if ( pathTokens[0].trim().isEmpty() ) {
            // DS records must be a root JsonObject, so do NOT support paths that expect root level to be a JsonArray, root must be JsonObject
            return;
        } else if ( !element.isJsonObject() ) {
            return;
        }

        int index = -1;
        JsonElement traverser = element;
        JsonElement parent = element;
        String token = null;
        int i = 0;

        for( i = 0; i < pathTokens.length; i++ ) {

            Boolean lastPathToken = ( i == ( pathTokens.length - 1 ) );

            token = pathTokens[ i ];
            parent = traverser;

            // if there are more path tokens to traverse (!lastPathToken)
            // then peek ahead and force the nextPathToken
            // to be the proper type (array/object)
            // if it doesn't exist then create it,
            // if it exists but is not the expected type
            // then overwrite the existing instance with
            // the expected instance type (array/object)
            // if it exists and is the correct type traverse into it

            // tokens can only be either
            // 1) array index elements
            // 2) object members


            index = getArrayToken( token  );
            Boolean isArrayToken = ( index != -1 ) ? true : false;

            if ( isArrayToken ) {

                try {
                    traverser.getAsJsonArray().get( index );
                } catch ( IndexOutOfBoundsException e ) {
                    extendArray( traverser.getAsJsonArray(), index );
                }

                JsonElement arrayElem = traverser.getAsJsonArray().get( index );
                if ( !lastPathToken ) {

                    // there are more path tokens to traverse
                    // so peek ahead and force the nextPathToken
                    // to be the proper type (array/object)
                    // if it doesn't exist then create it,
                    // if it exists but is not the expected type
                    // then overwrite the existing instance with
                    // the expected instance type (array/object)

                    String nextPathToken = pathTokens[ i + 1 ];

                    if ( nextPathToken.contains("=") ) {
                        // nextPathToken expected to be an array
                        if ( arrayElem == null || arrayElem == JsonNull.INSTANCE || !arrayElem.isJsonArray() ) {
                            traverser.getAsJsonArray().set( index, new JsonArray() );
                            arrayElem = traverser.getAsJsonArray().get( index );
                        }
                        parent = traverser.getAsJsonArray();
                        traverser = arrayElem.getAsJsonArray();

                    } else {
                        // nextPathToken expected to be an object
                        if ( arrayElem == null || arrayElem == JsonNull.INSTANCE || !arrayElem.isJsonObject() ) {
                            traverser.getAsJsonArray().set( index, new JsonObject() );
                            arrayElem = traverser.getAsJsonArray().get( index );
                        }
                        parent = traverser.getAsJsonArray();
                        traverser = arrayElem.getAsJsonObject();
                    }

                } else {

                    // this is the last token in path
                    parent = traverser.getAsJsonArray();
                    traverser = arrayElem;
                    // reset token value to last array index for updateValue(..) arg use below
                    // when use case is this array index part is the last token in the path
                    token = String.valueOf( index );
                }

            } else if ( traverser.isJsonObject() ) {

                JsonElement objMember = traverser.getAsJsonObject().get( token );
                if ( !lastPathToken ) {

                    String nextPathToken = pathTokens[ i + 1 ];

                    if ( nextPathToken.contains("=") ) {
                        // nextPathToken expected to be an array
                        if ( objMember == null || objMember == JsonNull.INSTANCE || !objMember.isJsonArray() ) {
                            traverser.getAsJsonObject().add( token, new JsonArray() );
                            objMember = traverser.getAsJsonObject().get( token );
                        }
                        parent = traverser.getAsJsonObject();
                        traverser = objMember.getAsJsonArray();
                    } else {
                        // nextPathToken expected to be an object
                        if ( objMember == null || objMember == JsonNull.INSTANCE || !objMember.isJsonObject() ) {
                            traverser.getAsJsonObject().add( token, new JsonObject() );
                            objMember = traverser.getAsJsonObject().get( token );
                        }
                        parent = traverser.getAsJsonObject();
                        traverser = objMember.getAsJsonObject();
                    }

                } else {

                    // this is the last token in path
                    parent = traverser.getAsJsonObject();
                    traverser = objMember;
                }
            } else {

                // TODO: @honorcode - this code branch should NOT be a reachable condition anymore but if not (bug?) ..... do what?
                // TODO: @honorcode - err handler ?, logger ?, throw exception ?
                break; // TODO: @honorcode - remove 'else' ????
            }
        }

        if( ( parent.isJsonObject() || parent.isJsonArray() ) &&
                ( token != null ) &&
                ( value != null || delete ) ) {

            updateValue(value, parent, token, delete);
        }

        return;
    }

    private static JsonArray initialiseArray(int size) {
        JsonArray array = new JsonArray();
        for (int j = 0; j < size; j++) {
            array.add(JsonNull.INSTANCE);
        }
        JsonElement temp = new JsonObject();
        array.add(temp);
        return array;
    }

    private static void extendArray(JsonArray array, int size) {
        for (int j = array.size(); j < size; j++) {
            array.add(JsonNull.INSTANCE);
        }
        JsonElement temp = new JsonObject();
        array.add(temp);
    }

    private static void updateValue(JsonElement value, JsonElement parent, String token, boolean delete) {
        if( parent.isJsonObject() ) {
            JsonObject object = (JsonObject) parent;
            if( delete ) {
                object.remove( token );
            } else {
                object.add( token, value );
            }
        }
        else if( parent.isJsonArray() ) {
            JsonArray object = (JsonArray) parent;
            int size = object.size();
            int index = Integer.parseInt( token );

            if( delete ) {
                object.remove( index );
            } else {
                for( int i=size; i<=index; i++ ){
                    object.add( JsonNull.INSTANCE );
                }
                object.set( index, value );
            }
        }
    }

    // TODO: honorcode->alex question: this appears to never be used ?
    private static JsonElement getArrayElement(JsonElement traverser, String token) {

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

    private static int getArrayToken(String token) {
        // array tokens of path are expected to be parsed into
        // e.g. '=1' parsed from '[1]' or 'arr.=1' parsed into 2 separate tokens 'arr','=1'
        boolean isArray = token.startsWith("=");
        if (!isArray) return -1;
        try {
            String[] arrayTokenParts = token.split("=");
            int index = (Integer) Integer.parseInt( arrayTokenParts[ 1 ] );
            if ( index >= 0 ) {
                return index;
            }
            return -1;
        } catch (Exception e) {
            return -1;
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


    // TODO: honorcode->alex question: this appears to never be used ?
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