/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.graph.GraphAdapterBuilder;
import com.google.gson.graph.GraphAdapterBuilder.Factory;
import com.google.gson.graph.GraphAdapterBuilder.Graph;
import com.google.gson.typeadapters.RuntimeTypeAdapterFactory;
import cz.cvut.kbss.failures.Vocabulary;
import cz.cvut.kbss.failures.model.GPSPosition;
import cz.cvut.kbss.failures.model.RawMaterial;
import cz.cvut.kbss.failures.model.Structure;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import java.io.IOException;
import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ContainerFactory;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

/**
 *
 * @author joudy
 */
public class Testing {
       
    public static EntityManager getEM() {
       return TestEnvironment.getPersistenceConnector("ic-strufail");
    }
            
  
    public static void testQuery(EntityManager em) {
        em.clear();
        GPSPosition o;
        
        o = new GPSPosition();
        //o.setId("pos1");
        o.setHasAltitude(0.1);
        o.setHasLongitude(0.2);
        o.setHasLatitude(0.3);
        
        em.getTransaction().begin();
        em.persist(o);
        em.getTransaction().commit();
        
        String iri = Vocabulary.s_c_GPSPosition;
        
        Query tq = em.createNativeQuery(
                "SELECT ?s WHERE {?s a <" + iri + ">}");
    
        System.out.println( (tq != null && tq.getResultList().size() > 0) ? "OK" : "FAIL");
    }
    
    public static void testAutoId(EntityManager em) {
        
//        @Id(generated = true)
//        protected String id;
//        TestCreateOperations -> testGenerateID
        
        em.clear();
        GPSPosition o;
        
        o = new GPSPosition();
        //o.setId("pos1");
        o.setHasAltitude(0.1);
        o.setHasLongitude(0.2);
        o.setHasLatitude(0.3);
        
        em.getTransaction().begin();
        em.persist(o);
        em.getTransaction().commit();

        String id1 = o.getId();
        System.out.println("first id: "+id1);
        
        
        o = new GPSPosition();
        //o.setId("pos2");
        o.setHasAltitude(10.0);
        o.setHasLongitude(0.2);
        o.setHasLatitude(0.3);
        
        em.getTransaction().begin();
        em.persist(o);
        em.getTransaction().commit();
        
        String id2 = o.getId();
        System.out.println("second id: "+id2);
        
        System.out.println(id1.equals(id2) ? "FAIL" : "OK");
    }
    
    public static void testConstraintFail(EntityManager em) {
        em.clear();
        Structure s = new Structure();
        //s.setId("stru1");
        s.setHasHistoricalEraSpecification("modern");
        s.setHasDescription("desc");
        //s.setHasGPS(p);
        
        try {
        
        em.getTransaction().begin();
        em.persist(s);
        em.getTransaction().commit();
        
//        //Caused by: cz.cvut.kbss.jopa.model.IntegrityConstraintViolatedException: Violated min=1, max=1, for attribute=SingularAttribute[hasGPS] of object=<stru1>
        
       } catch (OWLPersistenceException ex) {
            Throwable t = ex;
            while (t != null) {
                System.out.println("=========");
                System.out.println(t.getMessage());
                String a = t.getMessage();
                if (a.startsWith("Violated")) {
                    String pat = "Violated min=(.+), max=(.+), for attribute=(.+)\\[(.+)\\] of object=<(.+)>";   
                    Pattern p = Pattern.compile(pat);

                    Matcher m = p.matcher(a);
                    m.find();  
                    for (int i = 1; i <= 5; i++) {
                        System.out.printf("%d: %s\n", i, m.group(i));
                    }
                }
               
                t = t.getCause();
            }
        }
    }
    
    public static void testSerializeCardinality() throws ClassNotFoundException, IOException {
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("modern");
        s.setHasDescription("desc");
        s.setHasGPS(p);
        
        Set<RawMaterial> set = new HashSet();
        RawMaterial r;
        
        r = new RawMaterial();
        r.setId("beton");
        set.add(r);
        
        r = new RawMaterial();
        r.setId("drevo");
        set.add(r);
        
        s.setHasMaterial(set);
        
        Gson gson = new Extractor("cz.cvut.kbss.failures.model").createGsonBuilder();

        String j = gson.toJson(s);
        System.out.println(j);
        
        // serializuje to hezky do js pole
    }
    
    public static void testCascadePersist(EntityManager em) {
        em.clear();
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("modern");
        s.setHasDescription("desc");
        s.setHasGPS(p);
        
        em.getTransaction().begin();
        em.persist(s);
        em.persist(p);
        em.getTransaction().commit();
        
        p = null;
        s = null;
        
        Gson gson = new Gson();
        
        p = em.find(GPSPosition.class, "pos1");
        System.out.println(gson.toJson(p));
        
        s = em.find(Structure.class, "stru1");
        System.out.println(gson.toJson(s));
    }
    
    public static void testFindObjectWithoutClass(EntityManager em) {
        em.clear();
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("modern");
        s.setHasDescription("desc");
        s.setHasGPS(p);
        
        em.getTransaction().begin();
        em.persist(s);
        em.persist(p);
        em.getTransaction().commit();
        
        p = null;
        s = null;
        
        Gson gson = new Gson();
        
        Object po = em.find(GPSPosition.class, "pos1");
        System.out.println(gson.toJson(po));
        
        Object so = em.find(Structure.class, "stru1");
        System.out.println(gson.toJson(so));
        
        po = em.find(Object.class, "pos1");
        System.out.println(gson.toJson(po));
        
        so = em.find(Object.class, "stru1");
        System.out.println(gson.toJson(so));
    }
    
    public static void testSimpleJson(EntityManager em) throws ClassNotFoundException {
        em.clear();
        
        String cls = "cz.cvut.kbss.failures.model.GPSPosition";
        String value = "{\"id\":\"pos1\",\"hasAltitude\":\"6\",\"hasLongitude\":\"10\",\"hasLatitude\":\"15\"}";

        Gson gson = new Gson();

        GPSPosition o = (GPSPosition) gson.fromJson(value, Class.forName(cls));
             
        em.getTransaction().begin();
        em.persist(o);
        em.getTransaction().commit();
        
        GPSPosition p = null;
        
        p = em.find(GPSPosition.class, "pos1");
        
        System.out.println(gson.toJson(p));
    }
    
    public static void testBadJson() throws ClassNotFoundException {
        
        String cls = "cz.cvut.kbss.failures.model.GPSPosition";
        String value = "{\"id\":\"pos1\",\"hasAltitude\":\"aaaa\",\"hasLongitude\":\"10\",\"hasLatitude\":\"15\"}";

        Gson gson = new Gson();

        GPSPosition o = (GPSPosition) gson.fromJson(value, Class.forName(cls));
        
        System.out.println(o.getId());
    }
  
    public static void testJsonGraph() throws ClassNotFoundException, IOException, ParseException {      
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("0x1");
        s.setHasDescription("desc");
        s.setHasGPS(p);

        Extractor ex = new Extractor("cz.cvut.kbss.failures.model"); 
        Gson gson = ex.createGsonBuilder();
        JSONObject schema = ex.getSchema();

        String j = gson.toJson(s);
        //j = "{\"0x1\":{\"hovno\":\"fekal\",\"$class\":\"cz.cvut.kbss.failures.model.Structure\",\"id\":\"stru1\",\"hasGPS\":{\"$ref\":\"0x2\"},\"hasHistoricalEraSpecification\":\"0x1\",\"hasDescription\":\"desc\"},\"0x2\":{\"$class\":\"cz.cvut.kbss.failures.model.GPSPosition\",\"id\":\"pos1\",\"hasAltitude\":0.1,\"hasLongitude\":0.2,\"hasLatitude\":0.3}}";
        System.out.println(j);
        
//        Map obj = (Map) parseJSON(j);
//        
//        // for each instance move properties not present in schema (class definition) to `properties` attribute
//        for (Map inst: (Collection<Map>)obj.values()) {
//            Map props = (Map) ((Map)schema.get(inst.get("$class"))).get("properties");
//            //System.out.println(props);
//            
//            for (String prop: (Set<String>)new HashSet(inst.keySet())) {
//                if (!props.containsKey(prop) && !(prop.equals("$class") || prop.equals("types") || prop.equals("properties"))) {
//                    Map properties;
//                    if (inst.containsKey("properties")) {
//                        properties = (Map) inst.get("properties");
//                    } else {
//                        properties = new LinkedHashMap();
//                        inst.put("properties", properties);
//                    }
//                    JSONArray arr = new JSONArray();
//                    arr.add(inst.get(prop));
//                    properties.put(prop, arr);
//                    inst.remove(prop);
//                }
//            }
//        }
//        
//        Gson printer = new GsonBuilder().setPrettyPrinting().create();
//        j = JSONValue.toJSONString(obj);
//        //System.out.println(j);
//        
//        System.out.println(((Structure)gson.fromJson(j, Structure.class)).getHasGPS().getId());
//        
//        System.out.println(gson.toJson(gson.fromJson(j, Structure.class)));
//        
//        obj = (Map) parseJSON(gson.toJson(gson.fromJson(j, Structure.class)));
//        
//        for (Map inst: (Collection<Map>)obj.values()) {
//            if (inst.containsKey("properties")) {
//                Map properties = (Map) inst.get("properties");
//                for (String prop: (Set<String>)properties.keySet()) {
//                    JSONArray arr = (JSONArray) properties.get(prop);
//                    inst.put(prop, arr.get(0));
//                }
//                inst.remove("properties");
//            }
//        }
//        
//        System.out.println(JSONValue.toJSONString(obj));
    }
    
    public static void testJsonGraphDeserialize(EntityManager em) throws ClassNotFoundException, IOException {
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("0x1");
        s.setHasDescription("desc");
        s.setHasGPS(p);

        Extractor ex = new Extractor("cz.cvut.kbss.failures.model"); 
        
        GsonBuilder gsonBuilder = new GsonBuilder()
            .setPrettyPrinting();

        //gsonBuilder.registerTypeAdapter(Placeholder.class, new PlaceholderDeserialiser());
        
        GraphAdapterBuilder adapter = new GraphAdapterBuilder();
        
        //RuntimeTypeAdapterFactory<Foo> factory = RuntimeTypeAdapterFactory.of(Foo.class, "$class");
        Class[] classes = Extractor.getClasses("cz.cvut.kbss.failures.model");
        for (int i = 0; i < classes.length; i++) {
            adapter.addType(classes[i]);
            gsonBuilder.registerTypeAdapterFactory(RuntimeTypeAdapterFactory.of(classes[i], "$class").registerSubtype(classes[i], classes[i].getCanonicalName()));
            //factory.registerSubtype(classes[i], classes[i].getCanonicalName());
        }
        
        //Class cls = Placeholder.class;
        //adapter.addType(cls);
        //gsonBuilder.registerTypeAdapterFactory(factory);
        
        Factory factory = adapter
                         .registerOn(gsonBuilder);
        Gson gson = gsonBuilder.create();

        
        //Placeholder ph = new Placeholder();
        //ph.value = s;
        //ph.next = new Placeholder();
        //ph.next.value = p;

        
//        
//        Type collectionType = new TypeToken<Collection<Object>>(){}.getType();
//        
//        collection = gson.fromJson(j, collectionType);
//        
//        int i = 0;
//        
//        GPSPosition x = null, y = null;
//        
//        for (Object o:collection) {
//            if (i == 0) {
//                x = (GPSPosition) o;
//            } else if (i == 1) {
//                y = ((Structure)o).getHasGPS();
//            }
//            System.out.printf("%d, %s\n", i++, o.toString());
//        }
//        
//        System.out.println(x.toString());
//        System.out.println(y.toString());
//        System.out.println(x == y);
        
        //String j = gson.toJson(s);
        String j = "{"+
"  \"0x1\": {"+
"    \"$class\": \"cz.cvut.kbss.failures.model.Structure\","+
"    \"id\": \"stru1\","+
"    \"hasGPS\": {"+
"      \"$ref\": \"0x2\""+
"    },"+
"    \"hasHistoricalEraSpecification\": \"0x1\","+
"    \"hasDescription\": \"desc\""+
"  },"+
"  \"0x2\": {"+
"    \"$class\": \"cz.cvut.kbss.failures.model.GPSPosition\","+
"    \"id\": \"pos1\","+
"    \"hasAltitude\": 0.1,"+
"    \"hasLongitude\": 0.2,"+
"    \"hasLatitude\": 0.3"+
"  },"+
"  \"0x3\": {"+
"    \"$class\": \"cz.cvut.kbss.failures.model.GPSPosition\","+
"    \"id\": \"pos2\","+
"    \"hasAltitude\": 1,"+
"    \"hasLongitude\": 2,"+
"    \"hasLatitude\": 3"+
"  }"+
"}";
//        
         System.out.println(j);
//        s = null;
        s = gson.fromJson(j, Structure.class);
        Graph graph = factory.graphThreadLocal.get();
        factory.graphThreadLocal.remove();
        
        System.out.println(graph.map.size());
                    
                    em.clear();
                    em.getTransaction().begin();
                    
    
        for (GraphAdapterBuilder.Element<?> el : graph.map.values()) {
            //System.out.printf("%s, %s\n", el.id, el.value.toString());
            em.persist(el.value);
        }
                   
                    em.getTransaction().commit();
                    
                    s = null;
                    
                    s = em.find(Structure.class, "stru1");
                    
                    System.out.println(gson.toJson(s));
    }
    
    public static void testSerializeFetchedEntity(EntityManager em) throws ClassNotFoundException, IOException {
        em.clear();
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        Structure s = new Structure();
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("0x1");
        s.setHasDescription("desc");
        s.setHasGPS(p);
            
        em.getTransaction().begin();
        em.persist(s);
        em.getTransaction().commit();
        
        p = null;
        s = null;
        
        s = em.find(Structure.class, "stru1");
        
        Extractor ex = new Extractor("cz.cvut.kbss.failures.model");
        Gson gson = ex.createGsonBuilder();
        
        System.out.println(gson.toJson(s));
    }
    
    public static void testGetSchema() throws ClassNotFoundException, IOException {
        Extractor ex = new Extractor("cz.cvut.kbss.failures.model");  
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        String json = gson.toJson(ex.getSchema());
        System.out.println(json);
    }
    
    
    public static void testUpdate(EntityManager em) {
        em.clear();
        
        GPSPosition p = new GPSPosition();
        //p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        em.getTransaction().begin();
        em.persist(p);
        em.getTransaction().commit();
        
        //p = em.find(GPSPosition.class, "pos1");
        
//        p.setHasAltitude(4.0);
//        p.setHasLongitude(5.0);
//        p.setHasLatitude(6.0);
//        
//        em.getTransaction().begin();
//        em.persist(p);
//        //em.merge(p);
//        em.getTransaction().commit();
        
//        p = em.find(GPSPosition.class, "pos1");
        
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        System.out.println(gson.toJson(p));
        
        
        GPSPosition px = new GPSPosition();
        px.setId(p.getId());
        px.setHasAltitude(8.0);
        px.setHasLongitude(9.0);
        px.setHasLatitude(10.0);
        
        //URI ctx = em.getCurrentPersistenceContext().getContexts().get(0).getUri();
        
        
        em.getTransaction().begin();
        //em.persist(p);
        //em.getCurrentPersistenceContext().mergeDetached(px, ctx);
        em.getCurrentPersistenceContext().mergeDetached(px);
        //em.merge(p);
        em.getTransaction().commit();
        
        px = em.find(GPSPosition.class, p.getId());
        
        System.out.println(gson.toJson(px));
    }
    
    public static void testPersistence() {
        EntityManager em = getEM();
        em.clear();
        
        GPSPosition p = new GPSPosition();
        p.setId("pos1");
        p.setHasAltitude(0.1);
        p.setHasLongitude(0.2);
        p.setHasLatitude(0.3);
        
        em.getTransaction().begin();
        em.persist(p);
        em.getTransaction().commit();
        
        //em.flush();
        em.close();
        
        em = getEM();
        
        p = em.find(GPSPosition.class, "pos1");
        
        System.out.println(p);
    }
    
    public static void testConverter() throws ClassNotFoundException, IOException {
        Extractor ex = new Extractor("cz.cvut.kbss.failures.model");  
        Converter conv = new Converter(ex.getClasses());
        
//        GPSPosition p = new GPSPosition();
//        p.setId("pos1");
//        p.setHasAltitude(0.1);
//        p.setHasLongitude(0.2);
//        p.setHasLatitude(0.3);
//        
//        Structure s = new Structure();
//        s.setId("stru1");
//        s.setHasHistoricalEraSpecification("0x1");
//        s.setHasDescription("desc");
//        s.setHasGPS(p);
        
//        String json = conv.toJson(s);
        
        String json = "{" +
"  \"0x1\": {" +
"    \"$class\": \"cz.cvut.kbss.failures.model.Structure\"," +
"    \"id\": \"stru1\"," +
"    \"hasGPS\": {" +
"      \"$ref\": \"0x2\"" +
"    }," +
"    \"hasHistoricalEraSpecification\": \"0x1\"," +
"    \"hasDescription\": \"desc\"" +
"  }," +
"  \"0x2\": {" +
"    \"$class\": \"cz.cvut.kbss.failures.model.GPSPosition\"," +
"    \"id\": \"pos1\"," +
"    \"hasAltitude\": 0.1," +
"    \"hasLongitude\": 0.2," +
"    \"hasLatitude\": 0.3" +
"  }," +
"  \"0x3\": {" +
"    \"$class\": \"cz.cvut.kbss.failures.model.GPSPosition\"," +
"    \"id\": \"pos1\"," +
"    \"hasAltitude\": 4.0," +
"    \"hasLongitude\": 5.0," +
"    \"hasLatitude\": 6.0" +
"  }" +
"}";
        
        System.out.println(json);
        
        Map<String, Object> fromJson = conv.fromJson(json);
        Iterator<String> it = fromJson.keySet().iterator();
        while (it.hasNext()) {
            String key = it.next();
            System.out.printf("%s: %s\n", key, fromJson.get(key).toString());
        }
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
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws ClassNotFoundException, IOException, ParseException {
        
        EntityManager em = getEM();
        //testFindObjectWithoutClass(em);
        
        //testAutoId(em);
//        testQuery(em);
    
        //testCascadePersist(em);
        //testGetSchema();
       

         // testConstraintFail(em);        
        //testSerializeCardinality();
//          testBadJson();
//        testSimpleJson(em);        
       // testJsonGraph();
     //   testJsonGraphDeserialize(em);
        
       testUpdate(em);
//        testPersistence();
        
       // testConverter();
        
        // serializace fetchnuty entity - serializuje celý entity graph
        //testSerializeFetchedEntity(em);
        
    
//        Manager m = new Manager(em);
        
        
        
//  Type mapType = new TypeToken<Map<String, Map<String, Integer>>>() {}.getType();
//  Map<String, Map<String, Integer>> map = new HashMap<String, Map<String, Integer>>();
//  Map<String, Integer> value1 = new HashMap<String, Integer>();
//  value1.put("lalala", 78);
//  value1.put("haha", 9999);
//  map.put("id", value1);
//    
//  Map<String, Integer> value2 = new HashMap<String, Integer>();
//  value2.put("nahhd", 121112);
//  value2.put("uuywss", 19987);
//  map.put("thing", value2);
//    
//  Map<String, Integer> value3 = new HashMap<String, Integer>();
//  map.put("other", value3);
//
//  Gson gson = new Gson();
//  String json = gson.toJson(map, mapType);
//  System.out.println(json);
//    
//  Map<String, Map<String, Integer>> deserializedMap = gson.fromJson(json, mapType);
//  System.out.println(deserializedMap);
    }   
}

