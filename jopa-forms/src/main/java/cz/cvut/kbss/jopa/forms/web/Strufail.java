package cz.cvut.kbss.jopa.forms.web;

import com.google.gson.Gson;
import cz.cvut.kbss.failures.model.GPSPosition;
import cz.cvut.kbss.failures.model.Structure;
import cz.cvut.kbss.jopa.forms.Extractor;
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.PostConstruct;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

@ManagedBean(name="strufail")
@SessionScoped
public class Strufail implements Serializable {
    private String cls;
    private String value;
//    private EntityManager pc;
    private int autoid = 1;
    
    private static EntityManager em = null;

    @PostConstruct
    public void init() throws ClassNotFoundException, IOException {
        Extractor ex = new Extractor("cz.cvut.kbss.failures.model");       
        
        value = "{}";
        cls = "cz.cvut.kbss.failures.model.GPSPosition";

//        pc = getEM();
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
    
//    public String[] getClasses() {
//        List<String> l = new LinkedList<String>();
//        try {
//            for (Class c : Extractor.getClasses("cz.cvut.kbss.failures.model")) {
//                l.add(c.getName());
//            }
//        } catch (Exception ex) {
//            Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, null, ex);
//        }
//        Collections.sort(l);
//        return l.toArray(new String[0]);
//    }
//    
//    public String[] listInstances(String cls) {
//        Manager m = new Manager(pc);
//        
//        return m.getDirectSubclass(cls).toArray(new String[0]);
//    }

}