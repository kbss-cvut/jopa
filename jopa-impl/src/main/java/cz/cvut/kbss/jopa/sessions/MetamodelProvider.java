package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

import java.util.Set;

/**
 * Created by ledvima1 on 4.2.15.
 */
interface MetamodelProvider {

    /**
     * Gets the metamodel
     * @return Metamodel
     */
    Metamodel getMetamodel();

    /**
     * Gets and managed types.
     * @return Unmodifiable set of managed classes
     */
    Set<Class<?>> getManagedTypes();

    /**
     * Checks whether the specified class is a managed (=entity) type.
     * @param cls The class to check
     */
    boolean isTypeManaged(Class<?> cls);
}
