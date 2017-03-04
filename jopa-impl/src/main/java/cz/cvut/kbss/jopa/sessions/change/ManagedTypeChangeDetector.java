package cz.cvut.kbss.jopa.sessions.change;

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class ManagedTypeChangeDetector implements ChangeDetector {

    private final MetamodelProvider metamodelProvider;

    ManagedTypeChangeDetector(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
    }

    @Override
    public boolean hasChanges(Object clone, Object original) {
        final Object idOrig = EntityPropertiesUtils.getPrimaryKey(original, metamodelProvider.getMetamodel());
        final Object idClone = EntityPropertiesUtils.getPrimaryKey(clone, metamodelProvider.getMetamodel());

        if (idOrig == null && idClone != null || idOrig != null && idClone == null) {
            return true;
        }
        return idClone != null && !idClone.equals(idOrig);
    }
}
