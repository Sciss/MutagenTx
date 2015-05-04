package de.sciss.mutagentx

import de.sciss.lucre.stm.{TxnLike, DataStore, DataStoreFactory}
import de.sciss.serial.{DataInput, DataOutput}

object CachedStore {
  def wrap(f: DataStoreFactory[DataStore]): DataStoreFactory[DataStore] = new FactoryWrap(f)

  private final class FactoryWrap(peer: DataStoreFactory[DataStore])
    extends DataStoreFactory[DataStore] {

    def open(name: String, overwrite: Boolean): DataStore = {
      val ds = peer.open(name, overwrite = overwrite)
      new StoreWrap(ds)
    }
  }

  private final class StoreWrap(peer: DataStore) extends DataStore {


    def put(keyFun: DataOutput => Unit)(valueFun: DataOutput => Unit)(implicit tx: TxnLike): Unit = {
      ???
    }

    def get[A](keyFun: DataOutput => Unit)(valueFun: DataInput => A)(implicit tx: TxnLike): Option[A] = ???

    def numEntries(implicit tx: TxnLike): Int = peer.numEntries

    def flatGet[A](keyFun: DataOutput => Unit)(valueFun: DataInput => Option[A])(implicit tx: TxnLike): Option[A] = ???

    def remove(keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean = ???

    def contains(keyFun: DataOutput => Unit)(implicit tx: TxnLike): Boolean = ???

    def close(): Unit = peer.close()
  }
}
