/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.TypeAdapter;
import com.google.gson.graph.GraphAdapterBuilder;
import com.google.gson.graph.GraphAdapterBuilder.Factory;
import com.google.gson.graph.GraphAdapterBuilder.Graph;
import com.google.gson.internal.bind.JsonTreeReader;
import com.google.gson.reflect.TypeToken;
import com.google.gson.typeadapters.RuntimeTypeAdapterFactory;
import cz.cvut.kbss.failures.model.GPSPosition;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.json.simple.JSONArray;
import org.json.simple.parser.ContainerFactory;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

/**
 *
 * @author joudy
 */
public class Converter {
    private Class[] classes;
    private Gson gson;
    Factory factory;
    
    public Converter(Class[] classes) {
        this.classes = classes;
        gson = createGsonBuilder();
    }
    
    private Gson createGsonBuilder() {
        GsonBuilder gsonBuilder = new GsonBuilder()
            .setPrettyPrinting();
        
        GraphAdapterBuilder adapter = new GraphAdapterBuilder();
                
        for (int i = 0; i < classes.length; i++) {
            adapter.addType(classes[i]);
            gsonBuilder.registerTypeAdapterFactory(RuntimeTypeAdapterFactory.of(classes[i], "$class").registerSubtype(classes[i], classes[i].getCanonicalName()));
        }       
        factory = adapter
                                              .registerOn(gsonBuilder);
        gson = gsonBuilder.create();
        
        return gson;
    }
        
    public JsonElement toJsonTree(Object o) {
        return gson.toJsonTree(o);
    }
    
    public String toJson(Object o) {
        return gson.toJson(o);
    }
    
    public Map<String,Object> fromJson(String json) {
        Map entities = null;
        try {
            entities = (Map) parseJSON(json);
        } catch (ParseException ex) {
            Logger.getLogger(Converter.class.getName()).log(Level.SEVERE, null, ex);
        }
                
        Iterator iterator = entities.keySet().iterator();
        LinkedHashMap linkedHashMap = new LinkedHashMap();

        while (iterator.hasNext()) {
            
            String key = (String) iterator.next();
            Map entity = (Map) entities.get(key);
            Class cls = null;
            try {
                cls = Class.forName((String) entity.get("$class"));
            } catch (ClassNotFoundException ex) {
                Logger.getLogger(Converter.class.getName()).log(Level.SEVERE, null, ex);
            }
            
            TypeAdapter<?> create = factory.create(gson, TypeToken.get((Type) cls));
            
            Graph graph = factory.graphThreadLocal.get();
            if (graph == null) {
                gson.fromJson(json, (Type) cls);
            }            
            
            JsonObject jo = new JsonObject();
            jo.addProperty("$ref", key);
            try {
                linkedHashMap.put(key, create.read(new JsonTreeReader(jo)));
            } catch (IOException ex) {
                Logger.getLogger(Converter.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        factory.graphThreadLocal.remove();
        
        return linkedHashMap;

    }
    
    public static Object parseJSON(String json) throws ParseException {
        JSONParser parser = new JSONParser();

        ContainerFactory orderedKeyFactory = new ContainerFactory() {
            @Override
            public Map createObjectContainer() {
              return new LinkedHashMap();
            }

            @Override
            public List creatArrayContainer() {
                return new JSONArray(); //new LinkedList()
            }

        };

        return parser.parse(json,orderedKeyFactory);  
    }
}
