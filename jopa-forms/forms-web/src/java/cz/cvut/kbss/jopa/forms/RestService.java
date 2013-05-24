package cz.cvut.kbss.jopa.forms;

import cz.cvut.kbss.jopa.model.EntityManager;
import java.io.IOException;
import javax.annotation.PostConstruct;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.core.Response;


@Path("/json")
public class RestService {
 
    private Manager m;
    
	@GET
	@Path("/object/{cls},{id}")
	public Response getMsg(@PathParam("cls") String cls, @PathParam("id") String id) {
		String output = m.getObjectJson(id, cls);
 
		return Response.status(200).entity(output).build();
	}
    
    private EntityManager pc;

    @PostConstruct
    public void init() throws ClassNotFoundException, IOException {
        Extractor ex = new Extractor();
        m = new Manager(Strufail.getEM());
    }
    
    private EntityManager getEM() {
        return TestEnvironment.getPersistenceConnector("ic-strufail");
    }
 
}