package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import cz.cvut.kbss.jopa.model.EntityManager;
import java.io.IOException;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

@ManagedBean(name="strufail")
@SessionScoped
public class Strufail implements Serializable {
    private String schema;
    private String cls;
    private String value;
    private EntityManager pc;
    private int autoid = 1;
    
    private static EntityManager em = null;

    @PostConstruct
    public void init() throws ClassNotFoundException, IOException {
        Extractor ex = new Extractor();       
        schema = ex.getSchema("cz.cvut.kbss.failures.model").toString();
        value = "{}";
        cls = "cz.cvut.kbss.failures.model.GPSPosition";

        pc = getEM();
        //pc.clear();
    }
    
    public static EntityManager getEM() {
        if (em == null) {
            em = TestEnvironment.getPersistenceConnector("ic-strufail");
        }
        return em;
    }
    
    /**
     * @return the schema
     */
    public String getSchema() {
        return schema;
    }

    /**
     * @param schema the schema to set
     */
    public void setSchema(String schema) {
        this.schema = schema;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return value;
    }

    /**
     * @param value the value to set
     */
    public void setValue(String value) {
        this.value = value;
        Class theClass = null;
        try {
            theClass = Class.forName(cls);
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        String pk;
        
        JSONObject jso = (JSONObject) JSONValue.parse(value);
        if (jso.containsKey("id")) {
            pk = (String) jso.get("id");
        } else {
            pk = ""+(autoid++);
            jso.put("id", pk);
        }
        this.value = jso.toString();
        
        
        Object o = null;
        Gson gson = new Gson();
        o = gson.fromJson(this.value, theClass);
       

        
        pc.getTransaction().begin();
        pc.persist(o);
        pc.flush();
        pc.getTransaction().commit();
        

        Object x = pc.find(theClass, pk);
        String s = new Manager(pc).getObjectJson(pk, cls);
        
        Logger.getLogger("").log(Level.INFO, "obj: "+s/*gson.toJson(x)*/);
        //System.out.println(x.getHasAltitude());

    }

    /**
     * @return the cls
     */
    public String getCls() {
        return cls;
    }

    /**
     * @param cls the cls to set
     */
    public void setCls(String cls) {
        this.cls = cls;
    }
    
    public String[] getClasses() {
        JSONObject o = (JSONObject) JSONValue.parse(schema);
        o = (JSONObject) o.get("definitions");
        List<String> l = new LinkedList<String>();
        for (Object k : o.keySet()) {
            l.add((String)k);
        }
        Collections.sort(l);
        return l.toArray(new String[0]);
    }
    
    public String[] listInstances(String cls) {
        Manager m = new Manager(pc);
        
        return m.getDirectSubclass(cls).toArray(new String[0]);
    }

}