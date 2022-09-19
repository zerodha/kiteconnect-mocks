// To parse this JSON:
//
//   NSError *error;
//   GttGetOrder * = [GttGetOrder fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class GttGetOrder;
@class GttGetOrderData;
@class GttGetOrderCondition;
@class GttGetOrderOrder;
@class GttGetOrderResult;
@class GttGetOrderOrderResult;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface GttGetOrder : NSObject
@property (nonatomic, nullable, strong) GttGetOrderData *data;
@property (nonatomic, nullable, copy)   NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface GttGetOrderData : NSObject
@property (nonatomic, nullable, strong) GttGetOrderCondition *condition;
@property (nonatomic, nullable, copy)   NSString *createdAt;
@property (nonatomic, nullable, copy)   NSString *expiresAt;
@property (nonatomic, nullable, strong) NSNumber *identifier;
@property (nonatomic, nullable, copy)   id meta;
@property (nonatomic, nullable, copy)   NSArray<GttGetOrderOrder *> *orders;
@property (nonatomic, nullable, copy)   id parentTrigger;
@property (nonatomic, nullable, copy)   NSString *status;
@property (nonatomic, nullable, copy)   NSString *type;
@property (nonatomic, nullable, copy)   NSString *updatedAt;
@property (nonatomic, nullable, copy)   NSString *userID;
@end

@interface GttGetOrderCondition : NSObject
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, strong) NSNumber *instrumentToken;
@property (nonatomic, nullable, strong) NSNumber *lastPrice;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSArray<NSNumber *> *triggerValues;
@end

@interface GttGetOrderOrder : NSObject
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, copy)   NSString *orderType;
@property (nonatomic, nullable, strong) NSNumber *price;
@property (nonatomic, nullable, copy)   NSString *product;
@property (nonatomic, nullable, strong) NSNumber *quantity;
@property (nonatomic, nullable, strong) GttGetOrderResult *result;
@property (nonatomic, nullable, copy)   NSString *tradingsymbol;
@property (nonatomic, nullable, copy)   NSString *transactionType;
@end

@interface GttGetOrderResult : NSObject
@property (nonatomic, nullable, copy)   NSString *accountID;
@property (nonatomic, nullable, copy)   NSString *exchange;
@property (nonatomic, nullable, copy)   NSString *meta;
@property (nonatomic, nullable, strong) GttGetOrderOrderResult *orderResult;
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

@interface GttGetOrderOrderResult : NSObject
@property (nonatomic, nullable, copy) NSString *orderID;
@property (nonatomic, nullable, copy) NSString *rejectionReason;
@property (nonatomic, nullable, copy) NSString *status;
@end

NS_ASSUME_NONNULL_END
