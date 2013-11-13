package cz.cvut.kbss.jopa.forms.web;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import cz.cvut.kbss.failures.model.GPSPosition;
import cz.cvut.kbss.failures.model.Structure;
import cz.cvut.kbss.jopa.forms.Converter;
import cz.cvut.kbss.jopa.forms.Extractor;
import cz.cvut.kbss.jopa.forms.Manager;
import cz.cvut.kbss.jopa.forms.TestEnvironment;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.OWLPersistenceException;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.PostConstruct;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;
import org.json.simple.parser.ParseException;


@Path("/json")
public class RestService {
    private static EntityManager em;
 
    private String namespace = "cz.cvut.kbss.failures.model";
    private Manager m;
    private Extractor ex;
    private Converter conv;
    private Gson gson;

    public static EntityManager getEM() {
        
        if (em == null) {
            em = TestEnvironment.getPersistenceConnector("ic-strufail");
            em.clear();
            
            GPSPosition p = new GPSPosition();
            //p.setId("pos1");
            p.setHasAltitude(0.1);
            p.setHasLongitude(0.2);
            p.setHasLatitude(0.3);
            

            Structure s = new Structure();
            //s.setId("stru1");
            s.setHasHistoricalEraSpecification("0x1");
            s.setHasDescription("desc");
            s.setHasGPS(p);
            
            
            
            try {
            
            em.getTransaction().begin();
            em.persist(p); 
            em.persist(s);
            em.getTransaction().commit();
            
            } catch (OWLPersistenceException ex) {
                Throwable t = ex;
                while (t != null) {
                    Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, "=========");
                    Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, t.getMessage());
                    String a = t.getMessage();
                    if (a.startsWith("Violated")) {
                        String pat = "Violated min=(.+), max=(.+), for attribute=(.+)\\[(.+)\\] of object=<(.+)>";   
                        Pattern pp = Pattern.compile(pat);

                        Matcher m = pp.matcher(a);
                        m.find();  
                        for (int i = 1; i <= 5; i++) {
                            Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, m.group(i));
                        }
                    }

                    t = t.getCause();
                }
            }
            
            Logger.getLogger(Strufail.class.getName()).log(Level.SEVERE, p.getId()+";"+s.getId());
        }
        return em;
    }
    
    
    
    @PostConstruct
    public void init() throws ClassNotFoundException, IOException {
        ex = new Extractor(namespace);
        m = new Manager(getEM());
        conv = new Converter(ex.getClasses());
        gson = new GsonBuilder().setPrettyPrinting().create();
    }
    
//    private EntityManager getEM() {
//        return TestEnvironment.getPersistenceConnector("ic-strufail");
//    }
    
	@GET
	@Path("/entity/{cls},{id}")
	public Response getEntity(@PathParam("cls") String cls, @PathParam("id") String id) {
        String output = "";
        try {
            //output = conv.toJsonString(em.find(Class.forName(cls), id));
            output = gson.toJson(em.find(Class.forName(cls), id));
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(RestService.class.getName()).log(Level.SEVERE, null, ex);
        }
 
		return Response.status(200).entity(output).build();
	}

    @POST
	@Path("/entity")
	public Response postEntity(String data) {

		Map<String, Object> fromJson = conv.fromJson(data);
        
        Map json = null;
        try {
            json = (Map) conv.parseJSON(data);
        } catch (ParseException ex) {
            Logger.getLogger(RestService.class.getName()).log(Level.SEVERE, null, ex);
        }
        Iterator<String> it = json.keySet().iterator();
        
        List<String> errors = new LinkedList();
        
        try {
        
        em.getTransaction().begin();
        
            while (it.hasNext()) {
                String key = it.next();
                Map obj = (Map) json.get(key);
                Object o = fromJson.get(key);
                if (obj.containsKey("id")) {
                    em.merge(o);
                } else {
                    em.persist(o);
                }
            }

        em.getTransaction().commit();
            
        } catch (OWLPersistenceException exe) {
            Throwable t = exe;
            while (t != null) {
                errors.add(t.getMessage());
                
                t = t.getCause();
            };

        }
        
        String out = "{\"entities: \"" + conv.toJson(fromJson) + ",\n\"errors\": " + conv.toJson(errors) + "}";
        
		return Response.status(200).entity(out).build();
	}
    
    @GET
    @Path("/schema")
    public Response getSchema() {
        String output = "";
        try {
            output = gson.toJson(ex.getSchema());
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(RestService.class.getName()).log(Level.SEVERE, null, ex);
        }
 
		return Response.status(200).entity(output).build();
    }
    
	@GET
	@Path("/instances/{iri}")
	public Response getInstance(@PathParam("iri") String iri) {
		String output = gson.toJson(m.getEntities(iri));
 
		return Response.status(200).entity(output).build();
	}

    @GET
	@Path("/subclasses/{iri}")
	public Response getSubclasses(@PathParam("iri") String iri) {
		String output = gson.toJson(m.getSubclasses(iri));
 
		return Response.status(200).entity(output).build();
	}
}