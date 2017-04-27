package atom_finder;

import ch.uzh.ifi.seal.changedistiller.treedifferencing.Node;
import ch.uzh.ifi.seal.changedistiller.model.classifiers.java.JavaEntityType;
import org.eclipse.cdt.core.dom.ast.IASTNode;

public class CDTChangeDistillerNode extends Node {
  private IASTNode node;

  public CDTChangeDistillerNode(IASTNode node) {
    super(JavaEntityType.WILDCARD_TYPE, node.getClass().getName());

    this.node = node;

    for (IASTNode child : node.getChildren()) {
      this.add(new CDTChangeDistillerNode(child));
    }
  }

  public IASTNode node() {
    return this.node;
  }
}
