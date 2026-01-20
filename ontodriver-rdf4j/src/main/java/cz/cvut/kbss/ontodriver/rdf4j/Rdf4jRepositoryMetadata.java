package cz.cvut.kbss.ontodriver.rdf4j;

import cz.cvut.kbss.ontodriver.RepositoryMetadata;

public class Rdf4jRepositoryMetadata implements RepositoryMetadata {

    private final String productName;

    public Rdf4jRepositoryMetadata(String productName) {this.productName = productName;}

    @Override
    public String getProductName() {
        return productName;
    }
}
