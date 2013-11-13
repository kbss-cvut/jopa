/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package cz.cvut.kbss.jopa.forms;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.query.Query;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author joudy
 */
public class Manager {
    private EntityManager em;

    public Manager(EntityManager em) {
        this.em = em;
    }

    public List<String> getEntities(String iri) {
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

    public List<String> getSubclasses(String iri) {
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
