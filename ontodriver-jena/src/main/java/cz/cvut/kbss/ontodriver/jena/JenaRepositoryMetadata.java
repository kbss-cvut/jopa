package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.RepositoryMetadata;

public class JenaRepositoryMetadata implements RepositoryMetadata {

    @Override
    public String getProductName() {
        return "Jena";
    }
}
