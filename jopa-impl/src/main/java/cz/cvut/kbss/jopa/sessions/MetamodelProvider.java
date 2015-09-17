package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

public interface MetamodelProvider {

    /**
     * Gets the metamodel
     * @return Metamodel
     */
    Metamodel getMetamodel();

    /**
     * Checks whether the specified class is a managed (=entity) type.
     * @param cls The class to check
     */
    boolean isTypeManaged(Class<?> cls);
}
