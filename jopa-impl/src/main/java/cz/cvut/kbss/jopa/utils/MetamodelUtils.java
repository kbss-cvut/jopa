package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.metamodel.Metamodel;

import java.net.URI;
import java.util.Collection;
import java.util.Objects;
import java.util.Set;

/**
 * Metamodel-related utility functions.
 */
public class MetamodelUtils {

    private MetamodelUtils() {
        throw new AssertionError();
    }

    /**
     * Checks whether the specified set of types contains any types not contained in the current module extraction
     * signature and if so, it adds them into the signature.
     *
     * @param types     The types to check (can be {@code null})
     * @param metamodel Persistence unit metamodel containing module extraction signature
     */
    public static void checkForModuleSignatureExtension(Collection<?> types, Metamodel metamodel) {
        Objects.requireNonNull(metamodel);
        if (types == null || types.isEmpty()) {
            return;
        }
        final Set<URI> signature = metamodel.getModuleExtractionExtraSignature();
        for (Object elem : types) {
            final URI u = EntityPropertiesUtils.getValueAsURI(elem);
            if (!signature.contains(u)) {
                metamodel.addUriToModuleExtractionSignature(u);
            }
        }
    }
}
