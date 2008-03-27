with Agpl.Containers.Bulk;

package Adafn.Transaction.containers is
new Agpl.Containers.Bulk (Transaction.Object,
                          "=",
                          Positive,
                          Identifier.Object,
                          Identifier."<",
                          Id);

