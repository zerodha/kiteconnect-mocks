// To parse this JSON:
//
//   NSError *error;
//   GttGetOrders * = [GttGetOrders fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class GttGetOrders;
@class GttGetOrdersDatum;
@class GttGetOrdersCondition;
@class GttGetOrdersMeta;
@class GttGetOrdersOrder;
@class GttGetOrdersResult;
@class GttGetOrdersOrderResult;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface GttGetOrders : NSObject
@property (nonatomic, nullable, copy) NSArray<GttGetOrdersDatum *> *data;
@property (nonatomic, nullable, copy) NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface GttGetOrdersDatum : NSObject
@property (nonatomic, nullable, strong) GttGetOrdersCondition *condition;
@property (nonatomic, nullable, copy)   NSString *createdAt;
@property (nonatomic, nullable, copy)   NSString *expiresAt;
@property (nonatomic, nullable, strong) NSNumber *identifier;
@property (nonatomic, nullable, strong) GttGetOrdersMeta *meta;
@property (nonatomic, nullable, copy)   NSArray<GttGetOrdersOrder *> *orders;
@property (nonatomic, nullable, copy)   id parentTrigger;
@property (nonatomic, nullable, copy)   NSString *status;
@property (nonatomic, nullable, copy)   NSString *type;
@property (nonatomic, nullable, copy)   NSString *updatedAt;
@property (nonatomic, nullable, copy)   NSString *userID;
@end

@interface GttGetOrdersCondition : NSObject
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, strong) NSNumber *instrumentToken;
@property (nonatomic, nullable, strong) NSNumber *lastPrice;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSArray<NSNumber *> *triggerValues;
@end

@interface GttGetOrdersMeta : NSObject
@end

@interface GttGetOrdersOrder : NSObject
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, copy)   NSString *orderType;
@property (nonatomic, nullable, strong) NSNumber *price;
@property (nonatomic, nullable, copy)   NSString *product;
@property (nonatomic, nullable, strong) NSNumber *quantity;
@property (nonatomic, nullable, strong) GttGetOrdersResult *result;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSString *transactionType;
@end

@interface GttGetOrdersResult : NSObject
@property (nonatomic, nullable, copy)   NSString *accountID;
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, copy)   NSString *meta;
@property (nonatomic, nullable, strong) GttGetOrdersOrderResult *orderResult;
@property (nonatomic, nullable, copy)   NSString *orderType;
@property (nonatomic, nullable, strong) NSNumber *price;
@property (nonatomic, nullable, copy)   NSString *product;
@property (nonatomic, nullable, strong) NSNumber *quantity;
@property (nonatomic, nullable, copy)   NSString *timestamp;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSString *transactionType;
@property (nonatomic, nullable, strong) NSNumber *triggeredAt;
@property (nonatomic, nullable, copy)   NSString *validity;
@end

@interface GttGetOrdersOrderResult : NSObject
@property (nonatomic, nullable, copy) NSString *orderID;
@property (nonatomic, nullable, copy) NSString *rejectionReason;
@property (nonatomic, nullable, copy) NSString *status;
@end

NS_ASSUME_NONNULL_END
