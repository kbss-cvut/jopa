/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import cz.cvut.kbss.failures.Vocabulary;
import cz.cvut.kbss.failures.model.GPSPosition;
import cz.cvut.kbss.failures.model.RawMaterial;
import cz.cvut.kbss.failures.model.Structure;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.query.Query;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

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
        s.setId("stru1");
        s.setHasHistoricalEraSpecification("modern");
        s.setHasDescription("desc");
        //s.setHasGPS(p);
        
        try {
        
        em.getTransaction().begin();
        em.persist(s);
        em.getTransaction().commit();
        
//        //Caused by: cz.cvut.kbss.jopa.model.IntegrityConstraintViolatedException: Violated min=1, max=1, for attribute=SingularAttribute[hasGPS] of object=<stru1>
        
        } catch (OWLPersistenceException ex) {
            System.out.println(ex.getCause().getCause());
            //IntegrityConstraintViolatedException
            System.out.println(ex.getLocalizedMessage());
       
 //            Throwable[] suppressed = ex.getSuppressed();
 //            System.out.println(suppressed.length);
 //            for (Throwable t:suppressed) {
 //                System.out.println(t.getLocalizedMessage());
 //            }
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
        
        Gson gson = new Extractor().createGsonBuilder("cz.cvut.kbss.failures.model");

        String j = gson.toJson(s);
        System.out.println(j);
        
        // serializuje to hezky do js pole
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
  
    public static void testJsonGraph() throws ClassNotFoundException, IOException {
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

        Gson gson = new Extractor().createGsonBuilder("cz.cvut.kbss.failures.model");

        String j = gson.toJson(s);
        System.out.println(j);
        
        System.out.print(((Structure)gson.fromJson(j, Structure.class)).getHasGPS().getId());
    }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws ClassNotFoundException, IOException {
        
        EntityManager em = getEM();
        
        testAutoId(em);
        testQuery(em);
    
        

//          testConstraintFail(em);        
//        testSerializeCardinality();
//          testBadJson();
//        testSimpleJson(em);        
//        testJsonGraph();
        
//        Extractor ex = new Extractor();
//        System.out.println(ex.getSchema("cz.cvut.kbss.failures.model").toString());         
//        Manager m = new Manager(em);
    }   
}
