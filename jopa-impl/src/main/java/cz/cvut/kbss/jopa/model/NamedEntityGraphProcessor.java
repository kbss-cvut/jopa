package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph;
import cz.cvut.kbss.jopa.model.metamodel.MetamodelClassMapper;

public class NamedEntityGraphProcessor {

    private final NamedEntityGraphManager manager;

    private final MetamodelClassMapper metamodel;

    public NamedEntityGraphProcessor(MetamodelClassMapper metamodel) {
        this.metamodel = metamodel;
        this.manager = new NamedEntityGraphManager();
    }

    public NamedEntityGraphManager getManager() {
        return manager;
    }

    public void buildEntityGraph(NamedEntityGraph declaration) {

    }
}
