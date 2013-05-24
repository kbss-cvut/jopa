/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.kbss.jopa.forms;

import com.google.gson.Gson;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author joudy
 */
public class Manager {
    private EntityManager em;

    public Manager(EntityManager em) {
        this.em = em;
    }

    public List<String> getObjPropPossibilities(String iri) {
        // TODO: plus get label
        Query tq = getEm().createNativeQuery(
                "SELECT ?s WHERE {?s a <" + iri + ">}");
        if (tq != null) {
            List<String> res = new ArrayList<String>();
            for (Object row : tq.getResultList()) {
                res.add((String) ((List) row).get(0));
            }
            return res;
        }
        return null;
    }

    public List<String> getDirectSubclass(String iri) {
        Query tq = getEm().createNativeQuery("PREFIX sdle: <http://pellet.owldl.com/ns/sdle#>" +
                "SELECT ?a WHERE { ?a sdle:directSubClassOf  <" + iri + "> }");
        if (tq != null) {
            List<String> res = new ArrayList<String>();
            for (Object row : tq.getResultList()) {
                res.add((String) ((List) row).get(0));
            }
            return res;
        }
        return null;
    }

    public String getObjectJson(String pk, String cls) {
        Object o = null;
        try {
            o = em.find(Class.forName(cls), pk);
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(Manager.class.getName()).log(Level.SEVERE, null, ex);
        }
        Gson gson = new Gson();
        return gson.toJson(o);
    }

    /**
     * @return the em
     */
    public EntityManager getEm() {
        return em;
    }

    /**
     * @param em the em to set
     */
    public void setEm(EntityManager em) {
        this.em = em;
    }
}
